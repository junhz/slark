package junhz
package http

import parser._

trait TransferCodings { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._

  val transfer_coding = token ^ ((ows ^ ";" ^ ows) :^ parameter).*

  trait TransferCoding

  case class TrasferCodingRegistery(name: String)

}