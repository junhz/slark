package slark
package parser

trait Symbols[+P <: Parsers] {

  val parsers: P

}