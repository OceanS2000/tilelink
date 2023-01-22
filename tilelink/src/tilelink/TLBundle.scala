package tilelink

import chisel3._
import chisel3.util._

import scala.collection.immutable.SeqMap

object TLBundle {
  def hasData(x: TLChannel): Bool = {
    x match {
      case a: TLChannelA => !a.opcode(2)
      //    opcode === PutFullData    ||
      //    opcode === PutPartialData ||
      //    opcode === ArithmeticData ||
      //    opcode === LogicalData
      case b: TLChannelB => !b.opcode(2)
      //    opcode === PutFullData    ||
      //    opcode === PutPartialData ||
      //    opcode === ArithmeticData ||
      //    opcode === LogicalData
      case c: TLChannelC => c.opcode(0)
      //    opcode === AccessAckData ||
      //    opcode === ProbeAckData  ||
      //    opcode === ReleaseData
      case d: TLChannelD => d.opcode(0)
      //    opcode === AccessAckData ||
      //    opcode === GrantData
      case e: TLChannelE => false.B
    }
  }

  def opcode(x: TLChannel): Option[UInt] = {
    x.elements.get("opcode").map(_.asUInt)
  }

  def size(x: TLChannel): Option[UInt] = {
    x match {
      case a: TLChannelA => Some(a.size)
      case b: TLChannelB => Some(b.size)
      case c: TLChannelC => Some(c.size)
      case d: TLChannelD => Some(d.size)
      case e: TLChannelE => None
    }
  }

  def numBeatsMinus1(x: TLChannel): UInt = {
    x match {
      case a: TLChannelA =>
        (utils.UIntToOH1(a.size, a.parameter.sizeWidth) >> log2Ceil(
          a.parameter.dataWidth / 8
        )).asUInt
      case a: TLChannelB =>
        (utils.UIntToOH1(a.size, a.parameter.sizeWidth) >> log2Ceil(
          a.parameter.dataWidth / 8
        )).asUInt
      case a: TLChannelC =>
        (utils.UIntToOH1(a.size, a.parameter.sizeWidth) >> log2Ceil(
          a.parameter.dataWidth / 8
        )).asUInt
      case a: TLChannelD =>
        (utils.UIntToOH1(a.size, a.parameter.sizeWidth) >> log2Ceil(
          a.parameter.dataWidth / 8
        )).asUInt
      case _: TLChannelE => 0.U
    }
  }
}

class TLBundle(val parameter: TLLinkParameter) extends Record with chisel3.experimental.AutoCloneType {
  def a: DecoupledIO[TLChannelA] =
    elements("a").asInstanceOf[DecoupledIO[TLChannelA]]

  def b: DecoupledIO[TLChannelB] =
    elements
      .getOrElse("b", throw new NoTLCException("b", parameter))
      .asInstanceOf[DecoupledIO[TLChannelB]]

  def c: DecoupledIO[TLChannelC] =
    elements
      .getOrElse("c", throw new NoTLCException("c", parameter))
      .asInstanceOf[DecoupledIO[TLChannelC]]

  def d: DecoupledIO[TLChannelD] =
    elements("d").asInstanceOf[DecoupledIO[TLChannelD]]

  def e: DecoupledIO[TLChannelE] =
    elements
      .getOrElse("e", throw new NoTLCException("e", parameter))
      .asInstanceOf[DecoupledIO[TLChannelE]]

  val elements: SeqMap[String, DecoupledIO[Bundle]] =
    SeqMap[String, DecoupledIO[Bundle]](
      "a" -> DecoupledIO(new TLChannelA(parameter.channelAParameter)),
      "d" -> Flipped(DecoupledIO(new TLChannelD(parameter.channelDParameter)))
    ) ++ (
      if (parameter.hasBCEChannels)
        Seq(
          "b" -> Flipped(
            DecoupledIO(new TLChannelB(parameter.channelBParameter.get))
          ),
          "c" -> DecoupledIO(new TLChannelC(parameter.channelCParameter.get)),
          "e" -> DecoupledIO(new TLChannelE(parameter.channelEParameter.get))
        )
      else
        Seq()
    )

}
