package test

import slark.FailReason

object ThrownDiary extends Diary {
  
  val content = 
    Source(FailReason.causedBy(new Exception).mkString("\r\nCaused by: ")) :: 
    Source(FailReason.causedBy(new Exception("a", new Exception)).mkString("\r\nCaused By: ")) :: Nil
  
}