package test
package mylogger

object LogDiary {
  def main(args: Array[String]) {
    val a = 2

    log(info"1")
    log(warn"1 + 1 = $a")
    log(error"a + a = ${a + a}")

    log(error"""
        this 
        is 
        a 
        multi-line
        """, new Exception("a", new Exception))
  }

  val + = new {
    //println(Thread.currentThread().getStackTrace()(1))
    log(error"inside ValDef Block")
    //println("test.logger.LogDiary.+(LogDiary.scala:24)")
  }
  
  val f = {
    log(error"inside ValDef ")
  }
  
  log(error"init")
}