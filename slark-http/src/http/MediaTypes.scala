package junhz
package http

import parser._

trait MediaTypes { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._

  val media_type = token ^ "/" :^ token ^ ((";" ^ ows) :^ parameter).*

  trait MediaType

  case class MediaTypeRegistery(typeName: String, subTypeName: String, required: List[String], optional: List[String]) {
    //http://www.iana.org/assignments/media-types
  }

}