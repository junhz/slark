package test
package myuri

import symbols.parsers._

object CharReaderDiary extends Diary {

  val content = Source("a" parse "a") :: 
    Source("a" parse "A") :: 
    Source("a".ignoreCase parse "A") :: 
    Source("a".ignoreCase parse "b") ::  Nil

}