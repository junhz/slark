package test

import slark.parser._

package object myparser {

  object parsers extends Parsers with ReaderApi with CombinatorApi with CombinatorAst {
    type From = Char

    type Input = CharReader

    override def isSame(f1: From, f2: From): Boolean = f1.equals(f2)

    trait CharReader extends Reader with ReaderOpt[CharReader] {}

    class StringReader(str: String, index: Int) extends CharReader {
      override def atEnd = index >= str.length()
      override def head = if (atEnd) ??? else str.charAt(index)
      override lazy val tail = if (atEnd) ??? else new StringReader(str, index + 1)
      override def toString = "\""+str.substring(index)+"\""
    }

    implicit val singletonStringReaderBuilder: String => Input = new StringReader(_, 0)

    class IterableReader(it: Iterable[Char]) extends CharReader {
      override def atEnd = it.isEmpty
      override def head = if (atEnd) ??? else it.head
      override lazy val tail = if (atEnd) ??? else new IterableReader(it.tail)
      override def toString = it.toString
    }
    
    implicit val singletonIterableReaderBuilder: Iterable[Char] => Input = new IterableReader(_)

    class SParser(str: String) extends Parser[String] {
      override def parse(input: Input) = {
        input.startWith(str) match {
          case None => Fail(s"can't match $str")
          case Some(n) => Succ(str, n)
        }
      }

      override def toString = "\""+str+"\""
    }

    implicit val sParserBuilder = new SParser(_)
  }
}