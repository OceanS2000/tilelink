package tilelink

import chisel3.util.isPow2

object TLChannelParameter {
  // implicit val rw = upickle.default.macroRW[TLChannelParameter]

  def bundle(p: TLChannelParameter): TLChannel = {
    p match {
      case a: TileLinkChannelAParameter => new TLChannelA(a)
      case b: TileLinkChannelBParameter => new TLChannelB(b)
      case c: TileLinkChannelCParameter => new TLChannelC(c)
      case d: TileLinkChannelDParameter => new TLChannelD(d)
      case e: TileLinkChannelEParameter => new TLChannelE(e)
    }
  }
}

sealed trait TLChannelParameter

case class TileLinkChannelAParameter(
  addressWidth: Int,
  sourceWidth:  Int,
  dataWidth:    Int,
  sizeWidth:    Int)
    extends TLChannelParameter {
  require(addressWidth > 0)
  require(sourceWidth > 0)
  require(dataWidth > 0)
  require(sizeWidth > 0)
  require(dataWidth % 8 == 0, "Width of data field must be multiples of 8")
  require(
    isPow2(dataWidth / 8),
    "Width of data field in bytes must be power of 2"
  )
}

case class TileLinkChannelBParameter(
  addressWidth: Int,
  sourceWidth:  Int,
  dataWidth:    Int,
  sizeWidth:    Int)
    extends TLChannelParameter {
  require(addressWidth > 0)
  require(sourceWidth > 0)
  require(dataWidth > 0)
  require(sizeWidth > 0)
  require(dataWidth % 8 == 0, "Width of data field must be multiples of 8")
  require(
    isPow2(dataWidth / 8),
    "Width of data field in bytes must be power of 2"
  )
}

case class TileLinkChannelCParameter(
  addressWidth: Int,
  sourceWidth:  Int,
  dataWidth:    Int,
  sizeWidth:    Int)
    extends TLChannelParameter {
  require(addressWidth > 0)
  require(sourceWidth > 0)
  require(dataWidth > 0)
  require(sizeWidth > 0)
  require(dataWidth % 8 == 0, "Width of data field must be multiples of 8")
  require(
    isPow2(dataWidth / 8),
    "Width of data field in bytes must be power of 2"
  )
}

case class TileLinkChannelDParameter(
  sourceWidth: Int,
  sinkWidth:   Int,
  dataWidth:   Int,
  sizeWidth:   Int)
    extends TLChannelParameter {
  require(sourceWidth > 0)
  require(sinkWidth > 0)
  require(dataWidth > 0)
  require(sizeWidth > 0)
  require(dataWidth % 8 == 0, "Width of data field must be multiples of 8")
  require(
    isPow2(dataWidth / 8),
    "Width of data field in bytes must be power of 2"
  )
}

case class TileLinkChannelEParameter(sinkWidth: Int) extends TLChannelParameter {
  require(sinkWidth > 0)
}
