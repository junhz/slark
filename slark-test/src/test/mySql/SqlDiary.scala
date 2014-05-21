package test
package mySql

import slark.sql.Sqls

object SqlDiary extends Diary {

  val sqls = Sqls(new org.h2.Driver)
  import sqls._

  val conn = connect("jdbc:h2:mem:test", "sa", "pwd")

  val content = Source {
    val r = for (
      _ <- update"create table LANG (name varchar(10))";
      rs1 <- query"select 'JAVA' from dual union select 'SCALA' from dual";
      name1 <- rs1;
      _ <- update"insert into LANG (name) values ($name1)"
    ) yield {
      println(s"$name1 inserted")
      for (
          rs2 <- query"select name from LANG" prepare conn;
          name2 <- rs2;
          _ <- update"delete from LANG where name like $name2" prepare conn
      ) yield {
        println(s"$name2 deleted")
      }
    }
    (r prepare conn).execute match {
      case Succ(sl) => for (s <- sl) { s.execute }
      case Fail(msg) => println(msg)
    }
    conn.close

  } :: Nil

}