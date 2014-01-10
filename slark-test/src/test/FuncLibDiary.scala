package test

import slark.FuncLib._

object FuncLibDiary extends Diary {

  val content =
    Source((open2 `with` 0)(sum)(1, 2)) ::
    Source((open2 append as[Int])(sum)((1, 2), 3)) :: Nil

  val sum: (Int, Int, Int) => String = (a: Int, b: Int, c: Int) => s"$a + $b + $c"

}