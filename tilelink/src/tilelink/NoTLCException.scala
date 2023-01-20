package tilelink

import chisel3.ChiselException
private class NoTLCException(
  channel:       String,
  linkParameter: TLLinkParameter)
    extends ChiselException(
      s"call $channel in TLBundle is not present in a TL-UL or TL-UH bus:\n $linkParameter"
    )
