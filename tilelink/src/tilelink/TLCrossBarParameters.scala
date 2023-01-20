package tilelink

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BitSet
import chisel3.util.experimental.decode.decoder

case class TLCrossBarMasterLinkParameter(
  linkParameter: TLLinkParameter,
  adVisibility:  chisel3.util.experimental.BitSet,
  bceVisibility: chisel3.util.experimental.BitSet,
  srcIdRange:    TLIdRange)
case class TLCrossBarSlaveLinkParameter(
  linkParameter: TLLinkParameter,
  adVisibility:  chisel3.util.experimental.BitSet,
  bceVisibility: chisel3.util.experimental.BitSet,
  sinkIdRange:   TLIdRange,
  addressRange:  chisel3.util.experimental.BitSet)

object TLCrossBarParameter {
  // implicit val rw = upickle.default.macroRW[TLCrossBarParameter]
  private def assignIdRange(sizes: Seq[Int]) = {
    val pow2Sizes = sizes.map { z => if (z == 0) 0 else 1 << log2Ceil(z) }
    val tuples    = pow2Sizes.zipWithIndex.sortBy(
      _._1
    ) // record old index, then sort by increasing size
    val starts =
      tuples
        .scanRight(0)(_._1 + _)
        .tail // suffix-sum of the sizes = the start positions
    val ranges = (tuples.zip(starts)).map { case ((sz, i), st) =>
      (if (sz == 0) TLIdRange(0, 0) else TLIdRange(st, st + sz), i)
    }
    ranges.sortBy(_._2).map(_._1) // Restore original order
  }
}

case class TLCrossBarParameter(
  arbitrationPolicy: TLArbiterPolicy,
  masters:           Seq[TLCrossBarMasterLinkParameter],
  slaves:            Seq[TLCrossBarSlaveLinkParameter])
    extends chisel3.experimental.SerializableModuleParameter {

  slaves.map(_.addressRange).combinations(2).foreach { case Seq(a, b) =>
    require(!a.overlap(b), s"Address ranges of different slaves cannot overlap: $a, $b.")
  }

  private[tilelink] def adReachableIO  = masters.map { case TLCrossBarMasterLinkParameter(_, visibility, _, _) =>
    slaves.map { case TLCrossBarSlaveLinkParameter(_, severability, _, _, _) =>
      visibility.overlap(severability)
    }.toVector
  }.toVector
  private[tilelink] def bceReachableIO = masters.map { case TLCrossBarMasterLinkParameter(_, _, visibility, _) =>
    slaves.map { case TLCrossBarSlaveLinkParameter(_, _, severability, _, _) =>
      visibility.overlap(severability)
    }.toVector
  }.toVector

  private[tilelink] def adReachableOI       = adReachableIO.transpose
  private[tilelink] def bceReachableOI      = bceReachableIO.transpose
  private[tilelink] def srcIdRemapTable     =
    TLCrossBarParameter.assignIdRange(masters.map(_.srcIdRange.end))
  private[tilelink] def sinkIdRemapTable    =
    TLCrossBarParameter.assignIdRange(slaves.map(_.sinkIdRange.end))
  private[tilelink] def commonLinkParameter = TLLinkParameter.union(
    masters.map(_.linkParameter) ++ slaves.map(_.linkParameter): _*
  )
}
