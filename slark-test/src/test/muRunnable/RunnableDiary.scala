package test.muRunnable

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
    
    val some = runnable {
      (s: Some[_]) => println(s.get)
    }
    
    val none = runnable {
      (n: None.type) => println("none")
    }
    
    val option1 = runnable[Unit, Option[_]] {
      (_: Unit) => Some(1)
    }
    
    val option2 = runnable {
      (_: Unit) => Some(1)
    }
    
    deploy(option1 >> runnable { _ match { case s @ Some(_) => some; case _ => none } }, executor) 
    
  }
  
  val threadID = (_: Unit) => println(Thread.currentThread().getId())
  
}