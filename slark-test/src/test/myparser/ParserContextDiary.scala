package test
package myparser

import parsers._
import slark.FailReason

object ParserContextDiary extends Diary {

  val source = "allow: 200 ok, 404 not found"
  val a: Parser[String] = "a"
  val omg = new FailReason {
    override def toString = "omg"
  }
  val content = Source("allow: 200 ok".parse(source)) ::
    Source(("allow: 200 ok" | fail(omg :: Nil)) parse (source)) ::
    Source("allow: 404 not found".? parse source) ::
    Source(("a": Parser[String]).* parse "aa") ::
    Source(("a": Parser[String]).* parse "") ::
    Source(("a")(1, `>`) parse "aa") ::
    Source(("a")(1, `>`) parse "") ::
    Source(("a")(`<`, 2) parse "") ::
    Source(("a")(`<`, 2) parse "a") ::
    Source(("a")(`<`, 2) parse "aa") ::
    Source(("a")(`<`, 2) parse "aaa") ::
    Source(("aa" | "a").* parse "aaa") ::
    Source(("a" ^ "b") ^ ("c" ^ "d") parse "abcd") ::
    Source(("a" ^ "b") ^ ("c" ^ "d") parse "abd") ::
    Source("a".! parse "b") ::
    Source("a" :^ "b" parse "ab") ::
    Source("a" :^ "b" parse "b") ::
    Source("a".! :^ "b" parse "b") ::
    Source("a".! :^ "b" parse "a") ::
    Source("a" ^: "b" parse "ab") ::
    Source("a" ^: "b" parse "aa") ::
    Source("a" ^: "b".! parse "ab") ::
    Source("a" ^: "b".! parse "aa") ::
    Source(a -> (_.length()) parse "a") ::
    Source(a -> (_.length()) parse "b") ::
    Source(a -> (_.charAt(2)) parse "a") ::
    Source(fail(omg :: Nil) | succ("2") parse "3") :: Nil

  println(a{ 1 << 21 } -> (_.length) parse new IteratorReader('a'))
  println((a.!){ 1 << 21 } -> (_.length) parse "")
}