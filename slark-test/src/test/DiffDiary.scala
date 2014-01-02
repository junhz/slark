package test

import slark.Diff._

object DiffDiary extends Diary {

  val content = 
    Source(diff("", "")) :: Source(diff("", "1")) :: Source(diff("1", "")) :: Source(diff("1", "1")) :: 
    Source(diff("abcabba", "cbabac")) :: Source(diff("cbabac", "abcabba")) :: Nil
  
  def diff(source: String, modified: String): Scripts = MyersDiff(source, modified)(_ equals _)
  
}