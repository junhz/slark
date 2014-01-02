package slark.parser

trait Symbols[+P <: Parsers] {

  val parsers: P

}