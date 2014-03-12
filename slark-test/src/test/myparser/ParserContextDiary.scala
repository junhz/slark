package test
package myparser

import parsers._

object ParserContextDiary extends Diary {

  val source = "allow: 200 ok, 404 not found"
  val a: Parser[String] = "a"
  val content = Source("allow: 200 ok".parse(source)) ::
    Source(("allow: 200 ok" | fail("omg")) parse (source)) ::
    Source("allow: 404 not found".? parse source) ::
    Source(("a": Parser[String]).* parse "aa") ::
    Source(("a": Parser[String]).* parse "") ::
    Source(1("a").+ parse "aa") ::
    Source(1("a").+ parse "") ::
    Source(2("a").- parse "") ::
    Source(2("a").- parse "a") ::
    Source(2("a").- parse "aa") ::
    Source(2("a").- parse "aaa") ::
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
    Source(fail("1") | succ("2") parse "3") :: Nil

  println((1 << 21)(a).fixed -> (_.length) parse new IteratorReader('a'))
  println((1 << 21)(a.!).fixed -> (_.length) parse "")
}