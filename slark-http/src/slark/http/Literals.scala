package slark
package http

import combinator.parser._

trait Literals {
  
  type P <: Parsers with OctetReaders
  val parsers: P
  import parsers._

  val upalpha = letter('A', 'Z') | fail(InvalidCharacter("upper case alpha"))

  val loalpha = letter('a', 'z') | fail(InvalidCharacter("lower case alpha"))

  val alpha = upalpha | loalpha | fail(InvalidCharacter("alhpa"))

  val digit = letter('0', '9') | fail(InvalidCharacter("digit"))

  val hex = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | fail(InvalidCharacter("hex"))

  val del = acsii(127)

  val cr = acsii(13)

  val lf = acsii(10)

  val sp = acsii(32)

  val ht = acsii(9)

  val crlf = cr ^ lf

  val vchar = %(0x21, 0x7E)
  
  val obs_text = %(0x80, 0xFF)
  
  val ows = (sp | ht).*

  val rws = (sp | ht)(1, `>`)

  val tchar = ('!': Parser[Byte]) | '#' | '$' | '%' | '&' | ''' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' | digit |alpha | fail(InvalidCharacter("tchar"))
  
  val token = tchar(1, `>`) -> (_.foldLeft(new StringBuilder)((sb, b) => { sb.append(b.toChar); sb }).toString)

  // \a = a \\ = \ \" = " \( = ( ") = )
  val quoted_pair = "\\" :^ (ht | sp | vchar | obs_text)

  val ctext = ht | sp | %(0x21, 0x27) | %(0x2A, 0x5B) | %(0x5D, 0x7E)  | obs_text | fail(InvalidCharacter("ctext"))

  // '\' is not allowed in quoted-text to live with quoted-pair
  val qdtext = ht | sp | '!' | %(0x23, 0x5B) | %(0x5D, 0x7E)  | obs_text | fail(InvalidCharacter("qdtext"))

  // don't allow nested comment
  val comment = '(' :^ ctext.* ^: ')'

  val quoted_string = '"' :^ qdtext.* ^: '"'
  
  def send(code: Int, reason: String): Parser[Nothing] = fail(HttpFailReason(code, reason))
  
  case class InvalidCharacter(category: String) extends FailReason {
    override def toString = s"$category wanted"
  }
  
  case class HttpFailReason(code: Int, reason: String) extends FailReason {
    override def toString = s"$code $reason"
  } 
}