package slark
package uri

import combinator.parser._

trait Literals {

  type P <: Parsers with CharReaders
  val parsers: P
  import parsers._

  val alpha = %(0x41, 0x5A) | %(0x61, 0x7A) | fail(InvalidCharacter("alpha") :: Nil)

  val digit = %(0x30, 0x39) | fail(InvalidCharacter("digit") :: Nil)

  val hexdig = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | fail(InvalidCharacter("hex") :: Nil)

  val unreserved = alpha | digit | '-' | '.' | '_' | '~' | fail(InvalidCharacter("unreserved") :: Nil)

  val gen_delims = (':': Parser[Char]) | '/' | '?' | '#' | '[' | ']' | '@' | fail(InvalidCharacter("gen-delims") :: Nil)

  val sub_delims = ('!': Parser[Char]) | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | fail(InvalidCharacter("sub-delims") :: Nil)

  val reserved = gen_delims | sub_delims | fail(InvalidCharacter("reserved") :: Nil)

  val pct_encoded = ("%" :^ hex"$hexdig$hexdig") -> { i => i.toChar }

  val pchar = unreserved | pct_encoded | sub_delims | ':' | '@' | fail(InvalidCharacter("pchar") :: Nil)
  
  case class InvalidCharacter(category: String) extends FailReason {
    override def toString = s"$category wanted"
  }
}