package slark
package uri

import parser._

trait Literals { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with CharReader] =>

  import parsers._

  val alpha = %(0x41, 0x5A) | %(0x61, 0x7A) | fail("alpha letter wanted")

  val digit = %(0x30, 0x39) | fail("digit wanted")

  object Digit {
    def unapply(str: String): Option[Int] = {
      if (str.length() != 1) None
      else unapply(str.charAt(0))
    }

    def unapply(c: Char): Option[Int] = if (c >= '0' && c <= '9') Some(c - '0') else None
  }

  val hexdig = (digit | "A" | "B" | "C" | "D" | "E" | "F" | "a" | "b" | "c" | "d" | "e" | "f" | fail("hex digit wanted"))

  object HexDig {
    def unapply(c: Char): Option[Int] = {
      if (c >= '0' && c <= '9') Some(c - '0')
      else if (c >= 'A' && c <= 'F') Some(c - 'A')
      else if (c >= 'a' && c <= 'f') Some(c - 'a')
      else None
    }

    def unapply(str: String): Option[Int] = {
      if (str.length() != 1) None else unapply(str.charAt(0))
    }
  }

  val unreserved = alpha | digit | "-" | "." | "_" | "~" | fail("unreserved character wanted")

  val gen_delims = ":" | "/" | "?" | "#" | "[" | "]" | "@" | fail("gen-delims character wanted")

  val sub_delims = "!" | "$" | "&" | "'" | "(" | ")" | "*" | "+" | "," | ";" | "=" | fail("sub-delims character wanted")

  val reserved = gen_delims | sub_delims | fail("reserved character wanted")

  val pct_encoded = ("%" :^ hexdig ^ hexdig) -> { case (HexDig(p1), HexDig(p2)) => (p1 << 4 | p2).toChar.toString }

  object PercentEncode {
    def apply(c: Char): String = ???
  }

  val pchar = unreserved | pct_encoded | sub_delims | ":" | "@" | fail("pchar wanted")

  // TODO: replacing AltAST for better performance
  def %(start: Byte, end: Byte): Parser[String] = new Parser[String] {
    require(start >= 0 && end > start)

    override def parse(input: Input) = if (input.atEnd) Fail("at end of input") else {
      val cnt = input.head
      if (cnt >= start && cnt <= end) Succ(cnt.toString, input.tail)
      else Fail(s"$toString wanted but %${cnt.toInt.toHexString} found")
    }

    override val toString = s"%(${start.toInt.toHexString}, ${end.toInt.toHexString})"
  }
}