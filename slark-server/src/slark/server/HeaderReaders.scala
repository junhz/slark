package slark
package server

import combinator.parser._

trait HeaderReaders extends Readers[(String, List[Byte])] { self: Parsers =>
  
  final class ListHeaderReader(val headers: List[(String, List[Byte])]) extends Reader {
    override def head = if (atEnd) ??? else headers.head
    override lazy val tail = if (atEnd) ??? else new ListHeaderReader(headers.tail)
    override def atEnd = headers.isEmpty

    override def toString = headers.mkString("\r\n")
  }

  implicit val listHeaderReader: List[(String, List[Byte])] => Reader = new ListHeaderReader(_)
  
  type CharParsers <: Parsers with uri.CharReaders
  type ByteParsers <: Parsers with http.OctetReaders with http.ImportChars[CharParsers]
  type HttpSymbols <: Symbols[ByteParsers] with http.Literals with http.Message
  
  val httpSymbols: HttpSymbols
  
  final class HeaderParser[T](name: String, value: httpSymbols.parsers.Parser[T]) extends Parser[List[T]] {
    override def parse(input: Input) = {
      @tailrec
      def rec(headers: Input, collected: List[T], filtedOut: List[(String, List[Byte])]): Result = {
        if (headers.atEnd) Succ(collected.reverse, filtedOut.reverse)
        else headers.head match {
          case (n, v) if (n.equalsIgnoreCase(name)) => value parse httpSymbols.parsers.byteListOctetReader(v) match {
            case httpSymbols.parsers.Succ(r, n) if (n.atEnd) => rec(headers.tail, r :: collected, filtedOut)
            case _ => Fail("malformed header")
          }
          case _ => rec(headers.tail, collected, headers.head :: filtedOut)
        }
      }
      rec(input, Nil, Nil)
    }
  }
  
  implicit class Ops(name: String) {
    def `:`[T](p: httpSymbols.parsers.Parser[T]): Parser[T] = 
      new HeaderParser(name, p) >> { x => if(x.isEmpty || !x.tail.isEmpty) fail("no or more than one headers") else succ(x.head) }
      
    def `: #`[T](p: httpSymbols.parsers.Parser[T]): Parser[List[T]] = {
      val leading = httpSymbols.ows :^ p
      val tail = (httpSymbols.ows ^ httpSymbols.parsers.stringParser(",") ^ httpSymbols.ows) :^ p
      new HeaderParser(name, leading >> { x => (tail.*) -> { xs => x :: xs } }) -> (_.flatten) >> { x => if (x.isEmpty) fail("header not found") else succ(x) }
    }
  } 
  
}