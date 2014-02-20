package test
package myparser

import parsers._

object ParserContextDiary extends Diary {

  val source = stringReader("allow: 200 ok, 404 not found")
  val a: Parser[String] = "a"
  val content = Source("allow: 200 ok".parse(source)) ::
    Source(("allow: 200 ok" | fail("omg")) parse (source)) ::
    Source("allow: 404 not found".? parse source) ::
    Source(("a": Parser[String]).* parse stringReader("aa")) ::
    Source(("a": Parser[String]).* parse stringReader("")) ::
    Source(1("a").+ parse stringReader("aa")) ::
    Source(1("a").+ parse stringReader("")) ::
    Source(2("a").- parse stringReader("")) ::
    Source(2("a").- parse stringReader("a")) ::
    Source(2("a").- parse stringReader("aa")) ::
    Source(2("a").- parse stringReader("aaa")) ::
    Source(("aa" | "a").* parse stringReader("aaa")) ::
    Source(("a" ^ "b") ^ ("c" ^ "d") parse stringReader("abcd")) ::
    Source(("a" ^ "b") ^ ("c" ^ "d") parse stringReader("abd")) ::
    Source("a".! parse stringReader("b")) ::
    Source("a" :^ "b" parse stringReader("ab")) ::
    Source("a" :^ "b" parse stringReader("b")) ::
    Source("a".! :^ "b" parse stringReader("b")) ::
    Source("a".! :^ "b" parse stringReader("a")) ::
    Source("a" ^: "b" parse stringReader("ab")) ::
    Source("a" ^: "b" parse stringReader("aa")) ::
    Source("a" ^: "b".! parse stringReader("ab")) ::
    Source("a" ^: "b".! parse stringReader("aa")) ::
    Source(a -> (_.length()) parse stringReader("a")) ::
    Source(a -> (_.length()) parse stringReader("b")) ::
    Source(a -> (_.charAt(2)) parse stringReader("a")) ::
    Source(fail("1") | succ("2") parse stringReader("3")) :: 
    Source(((1 << 20)(a) parse stringReader(List.fill(1 << 20)("a").mkString)).isInstanceOf[ParseResult[_]]) :: Nil

}