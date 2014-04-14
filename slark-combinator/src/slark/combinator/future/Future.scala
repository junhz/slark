package slark
package combinator.future

import java.util.concurrent.ExecutorService

object Futures {

  sealed abstract class Future[T] {

    @tailrec
    final def deploy(service: ExecutorService): Unit = {
      val next = this match {
        case Done(fun) => Cache(fun) { new Runnable { override def run = fun() } }
        case Sync(future, fmap) => future match {
          case Done(fun) => Cache(fun, fmap) { new Runnable { override def run = fmap(fun()) } }
          case Sync(future, f) => Cache(f, fmap) { Sync(future, (t: Any) => fmap(f(t))) }
          case Async(future, f) => Cache(f, fmap) { Async(future, (t: Any) => fmap(f(t))) }
        }
        case Async(future, fmap) => future match {
          case Done(fun) => Cache(fun, fmap)(new Runnable {
            override def run = {
              val t = fun()
              service.execute(runs(t, fmap))
            }
          })
          case Sync(future, f) => Cache(f, fmap)(Sync(future, (t: Any) => {
            val u = f(t)
            service.execute(runs(u, fmap))
          }))
          case Async(future, f) => Cache(f, fmap)(Async(future, (t: Any) => Cache(f, fmap, service) {
            service.execute(new Runnable {
              override def run = {
                val u = f(t)
                service.execute(runs(u, fmap))
              }
            })
          }))
        }
      }
      next match {
        case r: Runnable => service.submit(r)
        case f: Future[T] => f.deploy(service)
      }
    }

    final def ->[U](f: T => U): Future[U] = Sync(this, f)

    final def ~>[U](f: T => U): Future[U] = Async(this, f)
  }

  private[this] case class Done[T](fun: () => T) extends Future[T]
  private[this] case class Sync[T, U](future: Future[T], fmap: T => U) extends Future[U]
  private[this] case class Async[T, U](future: Future[T], fmap: T => U) extends Future[U]

  private[this] def runs[T, U](t: T, f: T => U): Runnable = new Runnable {
    override def run = f(t)
  }

  def future[T](fun: () => T): Future[T] = Done(fun)
}