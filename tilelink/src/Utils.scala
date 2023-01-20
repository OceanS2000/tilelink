import chisel3._
import chisel3.util._

import scala.math.min

package object utils {
  def OH1ToOH(x: UInt): UInt =
    ((x << 1).asUInt | 1.U) & (~Cat(0.U(1.W), x)).asUInt

  def OH1ToUInt(x: UInt): UInt = OHToUInt(OH1ToOH(x))

  def UIntToOH1(x: UInt, width: Int): UInt =
    (~((-1).S(width.W).asUInt << x)(width - 1, 0)).asUInt

  def UIntToOH1(x: UInt): UInt = UIntToOH1(x, (1 << x.getWidth) - 1)

  def trailingZeros(x: Int): Option[Int] =
    if (x > 0) Some(log2Ceil(x & -x)) else None

  // Fill 1s from low bits to high bits
  def leftOR(x: UInt): UInt = leftOR(x, x.getWidth, x.getWidth)

  def leftOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)

    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s + s, x | (x << s) (width - 1, 0))

    helper(1, x)(width - 1, 0)
  }

  // Fill 1s form high bits to low bits
  def rightOR(x: UInt): UInt = rightOR(x, x.getWidth, x.getWidth)

  def rightOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)

    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s + s, x | (x >> s) (width - 1, 0))

    helper(1, x)(width - 1, 0)
  }
}
