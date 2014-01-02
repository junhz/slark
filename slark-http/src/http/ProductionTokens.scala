package junhz
package http

import parser._

trait ProductionTokens { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._

  val product = token ^ ('/' :^ token).?

}