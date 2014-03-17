package slark.combinator.parser

trait Symbols[+P <: Parsers] {

  protected[this] def _parsers: P
  
  final val parsers: P = _parsers

}