package slark
package uri

import parser._

trait IPaddress { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with CharReader with Formats] with Literals =>

  import parsers._

  /**
   * return Int from 0 - 255(0xfe)
   */
  val dec_octet =
    "25" :^ ("0" | "1" | "2" | "3" | "4" | "5") -> { case Digit(digits) => 250 + digits } |
      ("2" :^ ("0" | "1" | "2" | "3" | "4") ^ digit) -> { case (Digit(tens), Digit(digits)) => 200 + tens * 10 + digits } |
      ("1" :^ digit ^ digit) -> { case (Digit(tens), Digit(digits)) => 100 + tens * 10 + digits } |
      (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ^ digit) -> { case (Digit(tens), Digit(digits)) => tens * 10 + digits } |
      digit -> { case Digit(digits) => digits }

  /**
   * return Int from 0x0000 - 0xffff
   */
  val h16 = (hexdig ^ 3(hexdig).-) -> {
    case (HexDig(digits), Nil) => digits
    case (HexDig(tens), List(HexDig(digits))) => tens << 4 | digits
    case (HexDig(hundreds), List(HexDig(tens), HexDig(digits))) => hundreds << 8 | tens << 4 | digits
    case (HexDig(thousands), List(HexDig(hundreds), HexDig(tens), HexDig(digits))) => thousands << 12 | hundreds << 8 | tens << 4 | digits
  }

  /**
   * 32-bit
   */
  final class IPv4address(val byte1: Int, val byte2: Int, val byte3: Int, val byte4: Int) {
    override def toString = byte1+"."+byte2+"."+byte3+"."+byte4
  }

  val ipv4address = (dec_octet ^ "." :^ dec_octet ^ "." :^ dec_octet ^ "." :^ dec_octet) -> {
    case (((byte1, byte2), byte3), byte4) => new IPv4address(byte1, byte2, byte3, byte4)
  }

  object IPv4address {
    def unapply(ipv4: IPv4address): Option[(Int, Int, Int, Int)] = {
      import ipv4._
      Some((byte1, byte2, byte3, byte4))
    }
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
  final class IPv6address(dByte1: Int, dByte2: Int, dByte3: Int, dByte4: Int, dByte5: Int, dByte6: Int, dByte7: Int, dByte8: Int) {
    override def toString = hex"$dByte1:$dByte2:$dByte3:$dByte4:$dByte5:$dByte6:$dByte7:$dByte8".toLowerCase()
  }

  val ipv6address =
    (6(h16 ^: ":") ^ ls32) ->
      { case (List(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6, dByte7, dByte8) } |
      ("::" :^ 5(h16 ^: ":") ^ ls32) ->
      { case (List(dByte2, dByte3, dByte4, dByte5, dByte6), (dByte7, dByte8)) => new IPv6address(0, dByte2, dByte3, dByte4, dByte5, dByte6, dByte7, dByte8) } |
      (h16.? ^ "::" :^ 4(h16 ^: ":") ^ ls32) -> ^(^(?(0), __), __) ->
      { case ((dbyte1, List(dByte3, dByte4, dByte5, dByte6)), (dByte7, dByte8)) => new IPv6address(dbyte1, 0, dByte3, dByte4, dByte5, dByte6, dByte7, dByte8) } |
      ((h16 ^ 1(":" :^ h16).-).? ^ "::" :^ 3(h16 ^: ":") ^ ls32) -> ^(^(?((0, Nil)), __), __) -> ^(^(^(__, *(?(0))), __), __) ->
      { case (((dByte1, dByte2), List(dByte4, dByte5, dByte6)), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, 0, dByte4, dByte5, dByte6, dByte7, dByte8) } |
      ((h16 ^ 2(":" :^ h16).-).? ^ "::" :^ 2(h16 ^: ":") ^ ls32) -> ^(^(?((0, Nil)), __), __) -> ^(^(^(__, *(?(0), ?(0))), __), __) ->
      { case (((dByte1, (dByte2, dByte3)), List(dByte5, dByte6)), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, dByte3, 0, dByte5, dByte6, dByte7, dByte8) } |
      ((h16 ^ 3(":" :^ h16).-).? ^ "::" :^ h16 ^ ":" :^ ls32) -> ^(^(?((0, Nil)), __), __) -> ^(^(^(__, *(?(0), ?(0), ?(0))), __), __) ->
      { case (((dByte1, (dByte2, dByte3, dByte4)), dByte6), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, dByte3, dByte4, 0, dByte6, dByte7, dByte8) } |
      ((h16 ^ 4(":" :^ h16).-).? ^ "::" :^ ls32) -> ^(?((0, Nil)), __) -> ^(^(__, *(?(0), ?(0), ?(0), ?(0))), __) ->
      { case ((dByte1, (dByte2, dByte3, dByte4, dByte5)), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, dByte3, dByte4, dByte5, 0, dByte7, dByte8) } |
      ((h16 ^ 5(":" :^ h16).-).? ^ "::" :^ h16) -> ^(?((0, Nil)), __) -> ^(^(__, *(?(0), ?(0), ?(0), ?(0), ?(0))), __) ->
      { case ((dByte1, (dByte2, dByte3, dByte4, dByte5, dByte6)), dByte8) => new IPv6address(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6, 0, dByte8) } |
      ((h16 ^ 6(":" :^ h16).-).? ^: "::") -> ?((0, Nil)) -> ^(__, *(?(0), ?(0), ?(0), ?(0), ?(0), ?(0))) ->
      { case (dByte1, (dByte2, dByte3, dByte4, dByte5, dByte6, dByte7)) => new IPv6address(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6, dByte7, 0) }

  final class IPvFuture(version: String, address: String) {
    override def toString = s"v$version.$address"
  }

  val ipvFuture = ("v".ignoreCase :^ 1(hexdig).+ ^ "." :^ 1(unreserved | sub_delims | ":").+) -> { case (version, address) => new IPvFuture(version.mkString.toLowerCase(), address.mkString.toLowerCase()) }
}