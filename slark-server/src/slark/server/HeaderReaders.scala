package slark
package server

import combinator.parser._
import combinator.collector._

trait HeaderReaders { self: Collectors =>
  type Input = List[(String, List[Byte])]
  
  type CharParsers <: Parsers with uri.CharReaders
  type ByteParsers <: Parsers with http.OctetReaders with http.ImportChars[CharParsers]
  type HttpSymbols <: Symbols[ByteParsers] with http.Literals with http.Message
  
  val httpSymbols: HttpSymbols
  import httpSymbols._
  import parsers._
  
  final class HeaderCollector[T](name: String, value: Parser[T]) extends Collector[List[T]] {
    override def collect(input: Input) = {
      @tailrec
      def rec(headers: Input, collected: List[T], filtedOut: List[(String, List[Byte])]): CollectResult[List[T]] = {
        if (headers.isEmpty) collected match {
          case Nil => NotFound
          case _ => Collected(collected.reverse, filtedOut.reverse)
        }
        else headers.head match {
          case (n, v) if (n.equalsIgnoreCase(name)) => value parse v match {
            case Succ(r, n) if (n.atEnd) => rec(headers.tail, r :: collected, filtedOut)
            case _ => Malformed(name)
          }
          case _ => rec(headers.tail, collected, headers.head :: filtedOut)
        }
      }
      rec(input, Nil, Nil)
    }
  }
  
  implicit class Ops(name: String) {
    def `: `[T](p: Parser[T]): Collector[T] = 
      new HeaderCollector(name, p) flatMap { x => if(x.tail.isEmpty) collected(x.head) else malformed("no or more than one headers") }
      
    def `: #`[T](p: Parser[T]): Collector[List[T]] = {
      val leading = ows :^ p
      val tail = (ows ^ "," ^ ows) :^ p
      new HeaderCollector(name, leading >> { x => (tail.*) -> { xs => x :: xs } }) map (_.flatten) flatMap { x => if (x.isEmpty) malformed("no header value") else collected(x) }
    }
  } 
  
}