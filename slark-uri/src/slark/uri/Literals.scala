package slark
package uri

import combinator.parser._

trait Literals { self: Symbols[Parsers with CharReaders] =>

  import parsers._

  val alpha = %(0x41, 0x5A) | %(0x61, 0x7A) | fail("alpha letter wanted")

  val digit = %(0x30, 0x39) | fail("digit wanted")

  val hexdig = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | fail("hex digit wanted")

  val unreserved = alpha | digit | '-' | '.' | '_' | '~' | fail("unreserved character wanted")

  val gen_delims = p(':') | '/' | '?' | '#' | '[' | ']' | '@' | fail("gen-delims character wanted")

  val sub_delims = p('!') | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | fail("sub-delims character wanted")

  val reserved = gen_delims | sub_delims | fail("reserved character wanted")

  val pct_encoded = ("%" :^ hex"$hexdig$hexdig") -> { i => i.toChar }

  val pchar = unreserved | pct_encoded | sub_delims | ':' | '@' | fail("pchar wanted")
}