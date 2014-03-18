package slark
package http

import combinator.parser._

trait Literals { self: Symbols[Parsers with OctetReaders] =>
  import parsers._

  val char = acsii(0, 127)

  val upalpha = letter('A', 'Z') | fail("upper case alpha wanted")

  val loalpha = letter('a', 'z') | fail("lower case alpha wanted")

  val alpha = upalpha | loalpha | fail("alhpa wanted")

  val digit = letter('0', '9') | fail("digit wanted")

  val hex = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'

  val del = acsii(127)

  val ctl = acsii(0, 31) | del

  val cr = acsii(13)

  val lf = acsii(10)

  val sp = acsii(32)

  val ht = acsii(9)

  val crlf = cr ^ lf

  val ows = (sp | ht).?

  val rws = (sp | ht)

  val separator = sp | ht | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '[' | ']' | '?' | '=' | '{' | '}'

  val token = ((ctl | separator).! :^ char)(1, `>`)

  // \a = a \\ = \ \" = " \( = ( ") = )
  val quoted_pair = "\\" :^ char

  val ctext = ht | sp | %(0x21, 0x27) | %(0x2A, 0x5B) | %(0x5D, 0x7E) // | %(0x80, 0xFF)

  // '\' is not allowed in quoted-text to live with quoted-pair
  val qdtext = ht | sp | '!' | %(0x23, 0x5B) | %(0x5D, 0x7E) // | %(0x80, 0xFF)

  // don't allow nested comment
  val comment = '(' :^ ctext.* ^: ')'

  val quoted_string = '"' :^ qdtext.* ^: '"'

  // exclude qvalue
  val attribute = "q=".! :^ token

  val value = token | quoted_string

  val parameter = attribute ^ "=" :^ value
}