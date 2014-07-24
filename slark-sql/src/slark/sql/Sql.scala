package slark
package sql

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.language.higherKinds

trait Sqls {
  trait Result[+T]

  case class Succ[T](t: T) extends Result[T]

  case class Fail(msg: String) extends Result[Nothing]

  trait Connection {
    def update(sql: String, params: Seq[Any]): Statement[Int]

    def query(sql: String, params: Seq[Any]): Statement[List[Any]]

    def close: Unit

    def commit: Unit

    def rollback: Unit
  }

  def connect(url: String, user: String, password: String): Connection

  trait Statement[+T] { self =>
    def execute: Result[T]
    def flatMap[U](f: T => Statement[U]): Statement[U] = new Statement[U] {
      override def execute = self.execute match {
        case Succ(t) => f(t).execute
        case Fail(msg) => Fail(msg)
      }
    }
    def map[U](f: T => U): Statement[U] = new Statement[U] {
      override def execute = self.execute match {
        case Succ(t) => Succ(f(t))
        case Fail(msg) => Fail(msg)
      }
    }
  }

  trait Sql[+T] { self =>

    def prepare(conn: Connection): Statement[T]

    final def flatMap[U](fun: T => Sql[U]): Sql[U] = new Sql[U] {
      override def prepare(conn: Connection) = {
        self.prepare(conn) flatMap { (t: T) => fun(t).prepare(conn) }
      }
    }

    final def map[U](fun: T => U): Sql[U] = new Sql[U] {
      override def prepare(conn: Connection) = {
        self.prepare(conn) map fun
      }
    }

  }

  case class NULL(flag: Int)

  implicit class SqlContext(context: StringContext) {

    def update(params: Any*): Sql[Int] = new Sql[Int] {
      override def prepare(conn: Connection) = {
        conn.update(context.parts.mkString("?"), params)
      }
    }

    def query(params: Any*): Sql[List[Any]] = new Sql[List[Any]] {
      override def prepare(conn: Connection) = {
        conn.query(context.parts.mkString("?"), params)
      }
    }
  }

  implicit def revStatement[T](trav: List[Statement[T]]): Statement[List[T]] = new Statement[List[T]] {
    override def execute = {
      @tailrec
      def rec(rest: List[Statement[T]], processed: List[T]): Result[List[T]] = {
        if (rest.isEmpty) Succ(processed.reverse)
        else rest.head.execute match {
          case Succ(t) => rec(rest.tail, t :: processed)
          case Fail(msg) => Fail(msg)
        }
      }

      rec(trav, Nil)
    }
  }

  implicit def listIdentityStatement[T](s: Statement[T]): List[Statement[T]] = List(s)

  implicit def revSsql[T](trav: List[Sql[T]]): Sql[List[T]] = new Sql[List[T]] {
    override def prepare(conn: Connection): Statement[List[T]] = new Statement[List[T]] {
      override def execute: Result[List[T]] = {

        @tailrec
        def rec(rest: List[Sql[T]], processed: List[T]): Result[List[T]] = {
          if (rest.isEmpty) Succ(processed.reverse)
          else rest.head.prepare(conn).execute match {
            case Succ(t) => rec(rest.tail, t :: processed)
            case Fail(msg) => Fail(msg)
          }
        }

        rec(trav, Nil)
      }
    }
  }

  implicit def listIdentitySql[T](s: Sql[T]): List[Sql[T]] = List(s)
  
}

object Sqls {

  private[this] class GenSqls(driver: java.sql.Driver) extends Sqls {
    override def connect(url: String, user: String, password: String): Connection = {
      val p = new java.util.Properties
      p.put("user", user)
      p.put("password", password)
      val jConn = driver.connect(url, p)
      jConn.setAutoCommit(false)
      new Connection {
        def update(sql: String, params: Seq[Any]): Statement[Int] = {
          new Statement[Int] {
            override def execute = {
              try {
                val jStat = jConn.prepareStatement(sql)
                fill(jStat, params)
                val r = Succ(jStat.executeUpdate())
                jStat.close()
                r
              } catch {
                case e: java.sql.SQLException => Fail(e.getMessage())
              }
            }
          }
        }
        def query(sql: String, params: Seq[Any]): Statement[List[Any]] = {
          new Statement[List[Any]] {
            override def execute = {
              try {
                val jStat = jConn.prepareStatement(sql)
                fill(jStat, params)
                val rs = jStat.executeQuery()
                val meta = rs.getMetaData()
                val columNum = meta.getColumnCount()
                val rows = new ListBuffer[Any]
                while (rs.next()) {
                  val row = new ListBuffer[Any]
                  def rec(index: Int): Unit = {
                    if (index <= columNum) {
                      val a = meta.getColumnType(index) match {
                        case java.sql.Types.DATE => new java.util.Date(rs.getDate(index).getTime())
                        case java.sql.Types.TIMESTAMP => new java.util.Date(rs.getTimestamp(index).getTime())
                        case java.sql.Types.INTEGER => rs.getInt(index)
                        case java.sql.Types.VARCHAR => rs.getString(index)
                        case java.sql.Types.NUMERIC => BigDecimal(rs.getBigDecimal(index))
                      }
                      if (rs.wasNull()) row.append(NULL(meta.getColumnType(index)))
                      else row.append(a)

                      rec(index + 1)
                    }
                  }
                  rec(1)
                  rows append (toTuple(row.toList))
                }
                rs.close()
                jStat.close()
                Succ(rows.toList)
              } catch {
                case e: java.sql.SQLException => Fail(e.getMessage())
              }
            }
          }
        }

        override def close = jConn.close()

        override def commit = jConn.commit()

        override def rollback = jConn.rollback()

        private[this] final def fill(stat: java.sql.PreparedStatement, params: Seq[Any]): Unit = {
          def rec(index: Int, params: Seq[Any]): Unit = {
            if (!params.isEmpty) {
              params.head match {
                case i: Int => stat.setInt(index, i)
                case s: String => stat.setString(index, s)
                case d: java.util.Date => stat.setTimestamp(index, new java.sql.Timestamp(d.getTime()))
                case d: BigDecimal => stat.setBigDecimal(index, d.bigDecimal)
                case NULL(flag) => stat.setNull(index, flag)
              }

              rec(index + 1, params.tail)
            }
          }
          rec(1, params)
        }
      }
    }
  }

  def apply(driver: java.sql.Driver): Sqls = {
    driver.getClass().getCanonicalName() match {
      case "oracle.jdbc.driver.OracleDriver" => new GenSqls(driver)
      case "org.h2.Driver" => new GenSqls(driver)
      case _ => new GenSqls(driver)
    }
  }

  def toTuple(l: List[Any]): Any = {
    l match {
      case Nil => throw new IllegalArgumentException("no element")
      case x1 :: xs => xs match {
        case Nil => x1
        case x2 :: xs => xs match {
          case Nil => (x1, x2)
          case x3 :: xs => xs match {
            case Nil => (x1, x2, x3)
            case x4 :: xs => xs match {
              case Nil => (x1, x2, x3, x4)
              case x5 :: xs => xs match {
                case Nil => (x1, x2, x3, x4, x5)
                case x6 :: xs => xs match {
                  case Nil => (x1, x2, x3, x4, x5, x6)
                  case x7 :: xs => xs match {
                    case Nil => (x1, x2, x3, x4, x5, x6, x7)
                    case x8 :: xs => xs match {
                      case Nil => (x1, x2, x3, x4, x5, x6, x7, x8)
                      case x9 :: xs => xs match {
                        case Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9)
                        case x10 :: xs => xs match {
                          case Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
                          case _ => new IllegalArgumentException("list too long")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
