package test

import slark.FuncLib._

object FuncLibDiary extends Diary {

  val content =
    Source((open2 `with` 3 `then` sum)(1, 2)) ::
    Source((open2 and as[Int] `then` sum)((1, 2), 3)) :: 
    Source((open2 `then` cons[Int])(1, Nil)) :: 
    Source(optEx(None, 3)) :: 
    Source(optEx(Some(1, None), 3)) :: 
    Source(optEx(Some(1, Some(2)), 3)) :: 
    Source(List(((1, 2), 3)).map(as and as[Int] and as[Int] `then` sum apply _)) :: Nil
    
  val sum: (Int, Int, Int) => String = (a: Int, b: Int, c: Int) => s"$a + $b + $c"
  
  def cons[T]: (T, List[T]) => List[T] = (head: T, tail: List[T]) => head :: tail

  val optEx = ((as[Int] and as[Int].default(2)).default(1, 2) and as[Int] `then` sum)
}