package test
package myDiary

object DiaryDiary extends Diary {
  
  val k = 1
  
  override val content = Source {
    val j = 1
    val i = 1
    i + j
  } :: Source(1 + 1) :: Source("1") :: Source('1') :: Source(true) :: Source(false) :: Source(null) :: Source(()) :: Source(1) :: 
  Source(1l) :: Source(0x1) :: Source(1f) :: Source(1d) :: Source(k) :: Source(try throw new Exception catch { case _: Throwable => 1 }) :: 
  Source { val i = 1  } :: Source(a.i) :: Source(println(1)) :: Source("".charAt(1)) :: Nil
  
  class A {
    val i = 1
  }
  val a = new A

}