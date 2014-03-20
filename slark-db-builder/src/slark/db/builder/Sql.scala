package slark.db.builder

import scala.collection.immutable.Traversable
import java.sql.Connection
import java.sql.ResultSet
import java.sql.Statement
import java.sql.SQLException
import java.sql.PreparedStatement
import scala.collection.mutable.ListBuffer

trait Result[+T]

case class Succ[T](t: T, state: ConnState) extends Result[T]

case class Fail(msg: Any) extends Result[Nothing]

trait ConnState { self =>
  def conn: Connection

  def release: Unit

  def commit: Unit = {
    conn.commit()
    release
  }

  def discard: Unit = {
    conn.rollback()
    release
  }

  final def :+(rs: ResultSet): ConnState = new ConnState {
    override def conn = self.conn

    override def release = {
      rs.close()
      self.release
    }
  }

  final def :+(stat: Statement): ConnState = new ConnState {
    override def conn = self.conn

    override def release = {
      stat.close()
      self.release
    }
  }
  
}

object ConnState {
  def `new`(fun: => Connection): ConnState = new ConnState {
    override val conn = {
      val tmp = fun
      tmp.setAutoCommit(false)
      tmp
    }
    override def release = conn.close()
  }
}

trait Sql[+T] { self =>

  def run(state: ConnState): Result[T]

  final def flatMap[U](fun: T => Sql[U]): Sql[U] = new Sql[U] {
    override def run(state: ConnState) = self.run(state) match {
      case Succ(t, state) => fun(t).run(state)
      case f: Fail => f
    }
  }

  final def map[U](fun: T => U): Sql[U] = new Sql[U] {
    override def run(state: ConnState) = self.run(state) match {
      case Succ(t, state) => Succ(fun(t), state)
      case f: Fail => f
    }
  }
  
  final def filter(fun: T => Boolean): Sql[T] = withFilter(fun)
  
  final def withFilter(fun: T => Boolean): Sql[T] = new Sql[T] {
    override def run(state: ConnState) = self.run(state) match {
      case s @ Succ(t, state) if fun(t) => s
      case Succ(t, _) => Fail(s"match error on $t")
      case f => f
    }
  }
  
}

object Builder {

  case class NULL(flag: Int)

  implicit class SqlContext(context: StringContext) {
    /** don't care about return value */
    def update(params: Any*): Sql[Int] = new Sql[Int] {
      override def run(state: ConnState) = {
        val stat = state.conn.prepareStatement(context.parts.mkString("?"))
        println(context.parts.mkString("?"))
        prepare(stat, params)
        try {
          Succ(stat.executeUpdate(), { stat.close(); state })
        } catch {
          case e: SQLException => { stat.close(); Fail(e) }
        }
      }
    }
    /** return table */
    def query(params: Any*): Sql[Traversable[List[Any]]] = new Sql[Traversable[List[Any]]] {
      override def run(state: ConnState) = {
        val stat = state.conn.prepareStatement(context.parts.mkString("?"))
        println(context.parts.mkString("?"))
        prepare(stat, params)
        try {
          val rs = stat.executeQuery()
          Succ(new Traversable[List[Any]] {
            override def foreach[U](f: List[Any] => U): Unit = {

              val meta = rs.getMetaData()
              val columNum = meta.getColumnCount()
              while (rs.next()) {
                val buffer = new ListBuffer[Any]
                def rec(index: Int): Unit = {
                  if (index <= columNum) {
                    val a = meta.getColumnType(index) match {
                      case java.sql.Types.DATE => new java.util.Date(rs.getDate(index).getTime())
                      case java.sql.Types.INTEGER => rs.getInt(index)
                      case java.sql.Types.VARCHAR => rs.getString(index)
                      case java.sql.Types.NUMERIC => rs.getBigDecimal(index)
                    }
                    if (rs.wasNull()) buffer.append(NULL(meta.getColumnType(index)))
                    else buffer.append(a)

                    rec(index + 1)
                  }
                }
                rec(1)
                f(buffer.toList)
              }
            }
          }, state :+ stat :+ rs)
        } catch {
          case e: SQLException => { stat.close(); Fail(e) }
        }
      }
    }

    private[this] final def prepare(stat: PreparedStatement, params: Seq[Any]): Unit = {
      def rec(index: Int, params: Seq[Any]): Unit = {
        if (!params.isEmpty) {
          params.head match {
            case i: Int => stat.setInt(index, i)
            case s: String => stat.setString(index, s)
            case d: java.util.Date => stat.setDate(index, new java.sql.Date(d.getTime()))
            case NULL(flag) => stat.setNull(index, flag)
          }

          rec(index + 1, params.tail)
        }
      }
      rec(1, params)
    }

    /** return one values */
    def call(params: Any*): Sql[Any] = ???
  }

}