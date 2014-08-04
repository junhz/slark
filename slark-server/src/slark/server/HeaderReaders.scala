package slark
package server

import combinator.parser._
import combinator.parser.Parsers

trait HeaderReaders extends Readers.Indexed[String, List[List[Byte]]] { self: Parsers =>
  
  type CharParsers <: Parsers with uri.CharReaders
  type ByteParsers <: Parsers with http.OctetReaders with http.ImportChars[CharParsers]
  type HttpSymbols <: Symbols[ByteParsers] with http.Literals with http.Message
  
  val httpSymbols: HttpSymbols
  import httpSymbols.{ parsers => httpParsers, _ }
  import httpSymbols.parsers.byteListOctetReader
  import httpSymbols.parsers.stringParser
  
  final class MapReader(m: Map[String, List[List[Byte]]]) extends Reader {
    override def get(key: String): Option[(List[List[Byte]], Reader)] = {
      m.get(key.toLowerCase) match {
        case None => None
        case Some(bs) => Some((bs, new MapReader(m - key)))
      }
    }
  }
  
  implicit val mapReader: List[(String, List[Byte])] => MapReader = {
    headers => new MapReader(headers.groupBy(_._1.toLowerCase).map(e => (e._1, e._2.map(_._2))))
  }
  
  implicit class Ops(name: String) {
    def `: `[T](p: httpParsers.Parser[T]): Parser[Option[T]] = 
      new Parser[Option[T]] {
      override def parse(src: Input): Result = {
        src.get(name) match {
          case None => Fail("no header found")
          case Some((hs, rest)) => hs match {
            case h :: Nil => p.parse(h) match {
              case httpParsers.Succ(r, n) if (n.atEnd) => Succ(Some(r), rest)
              case _ => Succ(None, rest)
            }
            case _ => Fail("more than one headers found")
          }
        }
      }
    }
      
    def `: #`[T](p: httpParsers.Parser[T]): Parser[List[T]] = {
      val leading = ows :^ p
      val tail = (ows ^ "," ^ ows) :^ p
      val all = leading >> { x => (tail.*) -> { xs => x :: xs } }
      new Parser[List[T]] {
        override def parse(src: Input): Result = {
          src.get(name) match {
            case None => Fail("no header found")
            case Some((hs, rest)) =>  {
              @tailrec
              def rec(headers: List[List[Byte]], collected: List[List[T]]): Result = {
                if (headers.isEmpty) Succ(collected.reverse.flatten, rest)
                else all parse headers.head match {
                  case httpParsers.Succ(r, n) if (n.atEnd) => rec(headers.tail, r :: collected)
                  case _ => Fail(Nil, rest)
                }
              }
              rec(hs, Nil)
            }
          }
        }
      }
    }
  } 
  
}