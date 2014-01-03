package slark
package uri

import parser._

trait Literals { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with CharReader] =>

  import parsers._

  val alpha = %(0x41, 0x5A) | %(0x61, 0x7A) | fail("alpha letter wanted")

  val digit = %(0x30, 0x39) | fail("digit wanted")

  val hexdig = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | fail("hex digit wanted")

  val unreserved = alpha | digit | '-' | '.' | '_' | '~' | fail("unreserved character wanted")

  val gen_delims = p(':') | '/' | '?' | '#' | '[' | ']' | '@' | fail("gen-delims character wanted")

  val sub_delims = p('!') | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | fail("sub-delims character wanted")

  val reserved = gen_delims | sub_delims | fail("reserved character wanted")

  val pct_encoded = ("%" :^ hexdig ^ hexdig) -> { case (Natural0.Hex(p1), Natural0.Hex(p2)) => (p1 << 4 | p2).toChar }

  val pchar = unreserved | pct_encoded | sub_delims | ':' | '@' | fail("pchar wanted")

  def %(start: Byte, end: Byte): Parser[Char] = new Parser[Char] {
    require(start >= 0 && end > start)

    override def parse(input: Input) = if (input.atEnd) Fail("at end of input") else {
      val cnt = input.head
      if (cnt >= start && cnt <= end) Succ(cnt, input.tail)
      else Fail(s"$toString wanted but %${cnt.toInt.toHexString} found")
    }

    override def toString = s"%(${start.toInt.toHexString}, ${end.toInt.toHexString})"
  }
}