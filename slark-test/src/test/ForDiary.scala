package test

object ForDiary extends Diary {
  val i = 1 :: 2 :: 3 :: 4 :: Nil
  val content = Source {
    for(x <- i; if (x % 2) == 0;
    		j <- 0 to x
    )
      yield j
  } :: 
  Source(Source(())) :: Nil
}