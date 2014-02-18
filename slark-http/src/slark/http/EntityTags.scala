package slark.http

import parser._

trait EntityTags { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._
  
  val entity_tag = "W/".? ^ quoted_string

}