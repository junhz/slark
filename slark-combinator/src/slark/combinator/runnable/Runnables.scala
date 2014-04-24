package slark
package combinator.runnable

import java.lang.{ Runnable => JRunnable }
import java.util.concurrent.{ ExecutorService => JExecutorService }

object Runnables {

  implicit class ExecutorService(executor: JExecutorService) {
    def run[T](runnable: () => T): Unit = executor.submit(new JRunnable { override def run: Unit = runnable() })
    def shutdown: Unit = executor.shutdown()
  }

  sealed trait Runnable[+T] {
    final def map[U](f: T => U): Runnable[U] = FlatMap(this, (t: T) => Cache(f){ Identity(() => f(t)) })
    final def flatMap[U](f: T => Runnable[U]): Runnable[U] = FlatMap(this, f)
    final def filter(f: T => Boolean): Runnable[T] = FlatMap(this, (t: T) => { if(f(t)) instant(t) else NotRunnable })
    final def withFilter(f: T => Boolean): Runnable[T] = filter(f)
  }
  
  private[Runnables] abstract class InternalRunnable[T] extends Runnable[T]

  val NotRunnable: Runnable[Nothing] = new InternalRunnable[Nothing] {}

  private[this] case class Identity[T](f: () => T) extends InternalRunnable[T]
  
  private[this] case class Instant[T](f: T) extends InternalRunnable[T]

  private[this] case class FlatMap[T, U](r: Runnable[T], f: T => Runnable[U]) extends InternalRunnable[U]

  def deploy(r: Runnable[_], executor: ExecutorService): Unit = {
    @tailrec
    def rec(r: Runnable[_]): Unit = {
      val next = r match {
        case NotRunnable => ()
        case Identity(f) => executor.run(f)
        case Instant(t) => ()
        case FlatMap(r, f) => r match {
          case NotRunnable => ()
          case Identity(g) => Cache(f, g) { executor.run(() => deploy(f(g()), executor)) }
          case Instant(t) => f(t)
          case FlatMap(r, g) => Cache(f, g) { FlatMap(r, (t: Any) => FlatMap(g(t), f)) }
        }
      }

      next match {
        case () => ()
        case r: Runnable[_] => rec(r)
      }
    }
    
    rec(r)
  }
  
  def runnable[T](t: => T): Runnable[T] = Identity(() => t)
  
  def instant[T](t: T): Runnable[T] = Instant(t)

}