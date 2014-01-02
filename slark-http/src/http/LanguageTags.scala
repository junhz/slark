package junhz
package http

import parser._

trait LanguageTags { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._
  
  val tag = alpha ^ 7(alpha).-

  val language_tag = tag ^ ('-' :^ tag).*

}