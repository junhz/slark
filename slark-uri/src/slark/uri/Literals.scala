package slark
package uri

import combinator.parser._

trait Literals {

  type P <: Parsers with CharReaders
  val parsers: P
  import parsers._

  val alpha = %(0x41, 0x5A) | %(0x61, 0x7A) | fail(InvalidCharacter("alpha"))

  val digit = %(0x30, 0x39) | fail(InvalidCharacter("digit"))

  val hexdig = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | fail(InvalidCharacter("hex"))

  val unreserved = alpha | digit | '-' | '.' | '_' | '~' | fail(InvalidCharacter("unreserved"))

  val gen_delims = (':': Parser[Char]) | '/' | '?' | '#' | '[' | ']' | '@' | fail(InvalidCharacter("gen-delims"))

  val sub_delims = ('!': Parser[Char]) | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | fail(InvalidCharacter("sub-delims"))

  val reserved = gen_delims | sub_delims | fail(InvalidCharacter("reserved"))

  val pct_encoded = ("%" :^ hex"$hexdig$hexdig") -> { i => i.toChar }

  val pchar = unreserved | pct_encoded | sub_delims | ':' | '@' | fail(InvalidCharacter("pchar"))
  
  case class InvalidCharacter(category: String) extends FailReason {
    override def toString = s"$category wanted"
  }
}