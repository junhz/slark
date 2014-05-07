package test.myRunnable

import slark.combinator.runnable.Runnables._
import java.util.concurrent.Executors

object RunnableDiary {

  def main(args: Array[String]): Unit = {
    val exe = Executors.newFixedThreadPool(2)
    
    val f = runnable {
      (_: Unit) => Thread.currentThread().getId()
    }
    
    val g = f >> runnable {
      (i: Long) => Thread.currentThread().getId() :: i :: Nil
    }
    
    val h = g >> runnable {
      (l: List[Long]) => println(l.mkString(", "))
    }
    
    deploy(h, (), exe)
    
    Thread.sleep(1000)
    exe.shutdown()
    
  }
  
}