package test
package myuri

import symbols._
import parsers._

object LiteralsDiary extends Diary {

  val content = Source(("a" ^ ("b" | "c" | "d")) parse "") :: 
    Source(digit parse "") :: 
    Source(digit parse "1") :: 
    Source(hexdig parse "a") :: 
    Source(pct_encoded parse "%25") :: 
    Source(pct_encoded parse "%a") :: 
    Source(pct_encoded parse "%") :: Nil

}