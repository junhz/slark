package slark
package uri

import combinator.parser._
import FuncLib._;

trait IPaddress { self: Symbols[Parsers with ReaderApi with CharReader] with Literals =>

  import parsers._

  /**
   * return Int from 0 - 255(0xfe)
   */
  val dec_octet = (
    (p('2') ^ '5' ^ (p('0') | '1' | '2' | '3' | '4' | '5')) |
    (p('2') ^ (p('0') | '1' | '2' | '3' | '4') ^ digit) |
    (p('1') ^ digit ^ digit) |
    (succ('0') ^ (p('1') | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ^ digit) |
    succ('0') ^ succ('0') ^ digit) -> (as[Char] and as[Char] and as[Char]).varargs -> { case Natural0(i) => i }

  /**
   * return Int from 0x0000 - 0xffff
   */
  val h16 = hexdig(1, 4) -> { case Natural0.Hex(i) => i }

  /**
   * 32-bit
   */
  case class IPv4address(val byte1: Int, val byte2: Int, val byte3: Int, val byte4: Int) {
    override def toString = byte1+"."+byte2+"."+byte3+"."+byte4
  }

  val ipv4address = (dec_octet ^ "." :^ dec_octet ^ "." :^ dec_octet ^ "." :^ dec_octet) -> {
    case (((byte1, byte2), byte3), byte4) => IPv4address(byte1, byte2, byte3, byte4)
  }

  val ls32 =
    (h16 ^ ":" :^ h16) |
      ipv4address -> { case IPv4address(byte1, byte2, byte3, byte4) => (byte1 << 8 | byte2, byte3 << 8 | byte4) }

  implicit class NumericContext(context: StringContext) {
    def hex(nums: Int*): String = {
      context.s(nums.map(Integer.toHexString(_)): _*)
    }
  }

  // dByte here refer to 16-bit
  case class IPv6address(dByte1: Int, dByte2: Int, dByte3: Int, dByte4: Int, dByte5: Int, dByte6: Int, dByte7: Int, dByte8: Int) {
    override def toString = hex"$dByte1:$dByte2:$dByte3:$dByte4:$dByte5:$dByte6:$dByte7:$dByte8".toLowerCase()
  }

  val ipv6address = {
    val make = IPv6address.apply(_, _, _, _, _, _, _, _)
    val parser = (
      ((h16 ^: ":"){6} ^ ls32) -> (take6[Int] and open2[Int, Int] `then` make) |
      ("::" :^ (h16 ^: ":"){5} ^ ls32) -> (take5[Int] `with` 0 and open2[Int, Int] `then` make) |
      (h16.? ^ "::" :^ (h16 ^: ":"){4} ^ ls32) -> (as[Int].default(0) `with` 0 and take4[Int] and open2[Int, Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 1)).? ^ "::" :^ (h16 ^: ":"){3} ^ ls32) -> ((as[Int] and takeOption1(0)).default(0, 0) `with` 0 and take3[Int] and open2[Int, Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 2)).? ^ "::" :^ (h16 ^: ":"){2} ^ ls32) -> ((as[Int] and takeOption2(0)).default(0, 0, 0) `with` 0 and take2[Int] and open2[Int, Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 3)).? ^ "::" :^ h16 ^ ":" :^ ls32) -> ((as[Int] and takeOption3(0)).default(0, 0, 0, 0) `with` 0 and as[Int] and open2[Int, Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 4)).? ^ "::" :^ ls32) -> ((as[Int] and takeOption4(0)).default(0, 0, 0, 0, 0) `with` 0 and open2[Int, Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 5)).? ^ "::" :^ h16) -> ((as[Int] and takeOption5(0)).default(0, 0, 0, 0, 0, 0) `with` 0 and as[Int] `then` make) |
      ((h16 ^ (":" :^ h16)(`<`, 6)).? ^: "::") -> ((as[Int] and takeOption6(0)).default(0, 0, 0, 0, 0, 0, 0) `with` 0 `then` make))
    parser
  }

  final class IPvFuture(version: String, address: String) {
    override def toString = s"v$version.$address"
  }

  val ipvFuture = ("v".ignoreCase :^ hexdig(1, `>`) ^ "." :^ (unreserved | sub_delims | ':')(1, `>`)) -> { case (version, address) => new IPvFuture(version.mkString.toLowerCase(), address.mkString.toLowerCase()) }
}