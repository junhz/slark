package slark.http

import parser._

trait QualityValues { self: Symbols[CombinatorParsers with ReaderApi with OctetReader] with Literals =>
  
  import parsers._

  val qvalue = ("0" ^ ("." :^ 3(digit).-).?) | ("1" ^ ("." :^ 3('0').-).?)

}