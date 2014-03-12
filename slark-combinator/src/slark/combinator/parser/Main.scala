package slark.combinator.parser

import scala.collection.immutable.Iterable
import scala.collection.immutable.Stream

object Main extends Parsers with ReaderApi {

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
  
  class IteratorReader(c: Char) extends CharReader {
    override def atEnd = false
    override def head = c
    override lazy val tail = this
    override def toString = c.toString
  }

  implicit val singletonStringParserBuilder = (str: String) => parser { input =>
    input.startWith(str) match {
        case None => Fail(s"can't match $str")
        case Some(n) => Succ(str, n)
      }
  }

  def main(args: Array[String]): Unit = {
    println("a" parse "a")
    println("a" parse "")
    println("a" >> (_ => "b") parse "a")
    println("a" ^ "b" parse "ab")
    println("a" ^ "b" parse "ac")
    println("a" ^ "b" parse "bb")
    println("a".! parse "b")
    println("a".! parse "a")
    println("a".* parse "aa")
    println("a".* parse "")
    println(("a" | ("b" ^ ("c" | "d")) | "be") parse "be")
    println("a" { 1 << 21 } -> (_.length) parse new IteratorReader('a'))
    println("a".! { 1 << 21 } -> (_.length) parse "")
  }

}