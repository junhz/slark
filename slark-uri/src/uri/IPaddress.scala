package slark
package uri

import parser._

trait IPaddress { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with CharReader] with Literals =>

  import parsers._

  /**
   * return Int from 0 - 255(0xfe)
   */
  val dec_octet =
    "25" :^ ("0" | "1" | "2" | "3" | "4" | "5") -> { case Natural0(digits) => 250 + digits } |
      ("2" :^ ("0" | "1" | "2" | "3" | "4") ^ digit) -> { case (Natural0(tens), Natural0(digits)) => 200 + tens * 10 + digits } |
      ("1" :^ digit ^ digit) -> { case (Natural0(tens), Natural0(digits)) => 100 + tens * 10 + digits } |
      (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ^ digit) -> { case (Natural0(tens), Natural0(digits)) => tens * 10 + digits } |
      digit -> { case Natural0(digits) => digits }

  /**
   * return Int from 0x0000 - 0xffff
   */
  val h16 = (hexdig ^ 3(hexdig).-) -> { case (head, tail) => head :: tail } -> { case Natural0.Hex(i) => i }

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
  final class IPv6address(dByte1: Int, dByte2: Int, dByte3: Int, dByte4: Int, dByte5: Int, dByte6: Int, dByte7: Int, dByte8: Int) {
    override def toString = hex"$dByte1:$dByte2:$dByte3:$dByte4:$dByte5:$dByte6:$dByte7:$dByte8".toLowerCase()
  }

  object IPv6address {
    def compressed(prefix: List[Int], suffix: List[Int]): IPv6address = {
      val buf = (prefix ++: (0 +: 0 +: 0 +: 0 +: 0 +: 0 +: 0 +: 0 +: Vector.empty).drop(prefix.length)).dropRight(suffix.length) ++ suffix
      new IPv6address(buf(0), buf(1), buf(2), buf(3), buf(4), buf(5), buf(6), buf(7))
    }
  }

  def flatten(opt: Option[(Int, List[Int])]): List[Int] = opt match {
    case None => Nil
    case Some((head, tail)) => head :: tail
  }

  val ipv6address =
    (6(h16 ^: ":") ^ ls32) ->
      { case (List(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6), (dByte7, dByte8)) => new IPv6address(dByte1, dByte2, dByte3, dByte4, dByte5, dByte6, dByte7, dByte8) } |
      ("::" :^ 5(h16 ^: ":") ^ ls32) ->
      { case (dBytes2_6, (dByte7, dByte8)) => IPv6address.compressed(Nil, dBytes2_6 ::: dByte7 :: dByte8 :: Nil) } |
      (h16.? ^ "::" :^ 4(h16 ^: ":") ^ ls32) ->
      { case ((dbyte1, dBytes3_6), (dByte7, dByte8)) => IPv6address.compressed(dbyte1.getOrElse(0) :: Nil, dBytes3_6 ::: dByte7 :: dByte8 :: Nil) } |
      ((h16 ^ 1(":" :^ h16).-).? ^ "::" :^ 3(h16 ^: ":") ^ ls32) ->
      { case ((dBytes1_2, List(dByte4, dByte5, dByte6)), (dByte7, dByte8)) => IPv6address.compressed(flatten(dBytes1_2), dByte4 :: dByte5 :: dByte6 :: dByte7 :: dByte8 :: Nil) } |
      ((h16 ^ 2(":" :^ h16).-).? ^ "::" :^ 2(h16 ^: ":") ^ ls32) ->
      { case ((dBytes1_3, List(dByte5, dByte6)), (dByte7, dByte8)) => IPv6address.compressed(flatten(dBytes1_3), dByte5 :: dByte6 :: dByte7 :: dByte8 :: Nil) } |
      ((h16 ^ 3(":" :^ h16).-).? ^ "::" :^ h16 ^ ":" :^ ls32) ->
      { case ((dBytes1_4, dByte6), (dByte7, dByte8)) => IPv6address.compressed(flatten(dBytes1_4), dByte6 :: dByte7 :: dByte8 :: Nil) } |
      ((h16 ^ 4(":" :^ h16).-).? ^ "::" :^ ls32) ->
      { case (dBytes1_5, (dByte7, dByte8)) => IPv6address.compressed(flatten(dBytes1_5), dByte7 :: dByte8 :: Nil) } |
      ((h16 ^ 5(":" :^ h16).-).? ^ "::" :^ h16) ->
      { case (dBytes1_6, dByte8) => IPv6address.compressed(flatten(dBytes1_6), dByte8 :: Nil) } |
      ((h16 ^ 6(":" :^ h16).-).? ^: "::") ->
      { case dBytes1_7 => IPv6address.compressed(flatten(dBytes1_7), Nil) }

  final class IPvFuture(version: String, address: String) {
    override def toString = s"v$version.$address"
  }

  val ipvFuture = ("v".ignoreCase :^ 1(hexdig).+ ^ "." :^ 1(unreserved | sub_delims | ':').+) -> { case (version, address) => new IPvFuture(version.mkString.toLowerCase(), address.mkString.toLowerCase()) }
}