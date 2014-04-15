package test
package myFuture

import java.util.concurrent.Executors
import slark.combinator.future.Futures._

object FutureDiary {

  def main(args: Array[String]): Unit = {
    val exe = Executors.newFixedThreadPool(1)
    
    val f = future {
      println(Thread.currentThread().getId())
    } -> {
      threadID
    } +> {
      threadID
    } -> {
      threadID
    } +> {
      threadID
    }
    
    f.deploy(exe)
  }
  
  val threadID = (_: Unit) => println(Thread.currentThread().getId())
  
}