package slark
package combinator.runnable

import scala.language.implicitConversions
import java.util.concurrent.{ ExecutorService => JExecutorService, Callable, Future }

object Runnables {

  implicit class ExecutorService(executor: JExecutorService) {
    def run[T](runnable: () => T): Future[T] = executor.submit(new Callable[T] { override def call: T = runnable() })
    def shutdown: Unit = executor.shutdown()
  }

  sealed trait Runnable[-T, +R] {
    def >>[U](that: Runnable[R, U]): Runnable[T, U] = new >>(this, that)
  }
  
  private[Runnables] abstract class InternalRunnable[T, R] extends Runnable[T, R]

  // running to still
  val Still: Runnable[Any, Nothing] = new InternalRunnable[Any, Nothing] {}

  private[this] case class Identity[T, R](f: T => R) extends InternalRunnable[T, R]
  
  private[this] case class Instant[T, R](r: R) extends InternalRunnable[T, R]

  private[this] case class >>[T, R, U](r1: Runnable[T, R], r2: Runnable[R, U]) extends InternalRunnable[T, U]

  def deploy[T, R](r: Runnable[T, R], t: T, executor: ExecutorService): Unit = {
    
    @tailrec
    def rec(r: Runnable[Any, R], t: Any): Unit = {
      r match {
        case Still => ()
        case Identity(f) => executor.run(() => Cache(f) { f(t) })
        case Instant(t) => ()
        case r1 >> r2 => r1 match {
          case Still => ()
          case Identity(g) => Cache(r2, g) { executor.run(() => deploy(r2, g(t), executor)) }
          case Instant(tt) => rec(r2, tt)
          case r11 >> r12 => rec(new >>(r11, new >>(r12, r2)), t)
        }
      }
    }
    
    rec(r.asInstanceOf[Runnable[Any, R]], t)
  }

  def runnable[T, R](f: T => R): Runnable[T, R] = Identity(f)
  
}