package test

import slark.Thrown

object ThrownDiary extends Diary {
  
  val content = 
    Source(Thrown.cnt.mkString("\r\nCaused by: ")) :: 
    Source(Thrown.wrap(new Exception("a", new Exception)).mkString("\r\nCaused By: ")) :: Nil
  
}