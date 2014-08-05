package slark.combinator.parser

trait Symbols[+P <: Parsers] {

  val parsers: P

}