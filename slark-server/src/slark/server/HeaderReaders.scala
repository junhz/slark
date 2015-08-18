package slark
package server

import combinator.parser._
import combinator.parser.Parsers

trait HeaderReaders extends Readers.Indexed { self: Parsers =>

  type Key = String
  type Value = List[List[Byte]]
  type HttpSymbols <: http.Literals with http.Message

  val httpSymbols: HttpSymbols
  import httpSymbols._
  import encoder.encode
  import parsers.{ byteListOctetReader, stringParser }

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
    def `: `[T](p: parsers.Parser[T]): Parser[T] =
      new Parser[T] {
        override def parse(src: Input): ParseResult[T] = {
          src.get(name) match {
            case None => Fail(HeaderNotFound)
            case Some((hs, rest)) => hs match {
              case h :: Nil => p.parse(h) match {
                case parsers.Succ(r, n) if (n.atEnd) => Succ(r, rest)
                case _ => Fail(MalformedHeader)
              }
              case _ => Fail(DuplicatedHeader)
            }
          }
        }
      }

    def `: #`[T](p: parsers.Parser[T]): Parser[List[T]] = {
      val leading = ows :^ p
      val tail = (ows ^ "," ^ ows) :^ p
      val all = leading >> { x => (tail.*) -> { xs => x :: xs } }
      new Parser[List[T]] {
        override def parse(src: Input): ParseResult[List[T]] = {
          src.get(name) match {
            case None => Fail(HeaderNotFound)
            case Some((hs, rest)) => {
              @tailrec
              def rec(headers: List[List[Byte]], collected: List[List[T]]): ParseResult[List[T]] = {
                if (headers.isEmpty) Succ(collected.reverse.flatten, rest)
                else all parse headers.head match {
                  case parsers.Succ(r, n) if (n.atEnd) => rec(headers.tail, r :: collected)
                  case _ => Fail(DuplicatedHeader)
                }
              }
              rec(hs, Nil)
            }
          }
        }
      }
    }
  }

  case object HeaderNotFound extends FailReason
  case object DuplicatedHeader extends FailReason
  case object MalformedHeader extends FailReason

}