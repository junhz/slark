package test.muRunnable

import slark.combinator.runnable.Runnables._
import java.util.concurrent.Executors

object RunnableDiary {

  def main(args: Array[String]): Unit = {
    val exe = Executors.newFixedThreadPool(2)
    
    val f = runnable {
      println(Thread.currentThread().getId())
    } map {
      threadID
    }
    
    deploy(f, exe)
  }
  
  val threadID = (_: Unit) => println(Thread.currentThread().getId())
  
}