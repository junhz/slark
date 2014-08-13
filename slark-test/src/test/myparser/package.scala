package test

import slark.combinator.parser._
import slark.Readers
import slark.FailReason

package object myparser {

  object parsers extends Parsers with Readers.Linear {
    type T = Char
    
    class StringReader(str: String, index: Int) extends Reader {
      override def atEnd = index >= str.length()
      override def head = if (atEnd) ??? else str.charAt(index)
      override lazy val tail = if (atEnd) ??? else new StringReader(str, index + 1)
      override def toString = "\""+str.substring(index)+"\""
    }

    implicit val singletonStringReaderBuilder: String => Input = new StringReader(_, 0)

    class IterableReader(it: Iterable[Char]) extends Reader {
      override def atEnd = it.isEmpty
      override def head = if (atEnd) ??? else it.head
      override lazy val tail = if (atEnd) ??? else new IterableReader(it.tail)
      override def toString = it.toString
    }

    implicit val singletonIterableReaderBuilder: Iterable[Char] => Input = new IterableReader(_)

    class SParser(str: String) extends Parser[String] {
      override def parse(input: Input) = {
        input.startWith(str) match {
          case None => Fail(NotStartWith(str))
          case Some(n) => Succ(str, n)
        }
      }

      override def toString = "\""+str+"\""
    }

    implicit val sParserBuilder = new SParser(_)

    class IteratorReader(c: Char) extends Reader {
      override def atEnd = false
      override def head = c
      override lazy val tail = this
      override def toString = c.toString
    }

    case class NotStartWith(str: String) extends FailReason {
      override def toString = s"input not start with $str"
    }
  }
}