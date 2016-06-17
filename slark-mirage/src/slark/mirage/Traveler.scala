package slark.mirage

import java.sql.Connection
import java.sql.SQLException
import java.sql.SQLSyntaxErrorException
import scala.annotation.tailrec
import java.sql.ResultSet
import java.sql.PreparedStatement

/**
 * @author a554114
 */
class Traveler(val from: Connection, val to: Connection, val bypass: Set[String]) {
  def explore(m: Mirage): Unit = {
    var exp: Map[Mirage, Knowledge] = Map.empty
    var mirages = m :: Nil
    var visited = bypass
    while (!mirages.isEmpty) {
      while (!mirages.isEmpty) {
        println(mirages)
        val (ms, e) = explore(mirages.head, exp)
        mirages = ms ::: mirages.tail
        exp = e
      }

      val tables = exp collect {
        case (_, t: Knowledge.Table) if !visited.contains(t.name) => t
      }
      
      if (tables.isEmpty) {
        ()
      } else {
        tables foreach visit
        visited = visited ++ tables.map(_.name)
        mirages = m :: Nil
      }
    }
  }
  
  def explore(m: Mirage, exp: Map[Mirage, Knowledge]): (List[Mirage], Map[Mirage, Knowledge]) = {
    exp.get(m) match {
      case None => m.wander(from) match {
        case k@Knowledge.Table(_, name, columns, indexes, _) => {
          val colStr = columns.map(col => s"${col.name} ${col.dataType} ${if (col.nullable) "" else "NOT NULL"}").mkString(", ")
          ddl(s"create table $name ($colStr)")
          (Nil, exp.updated(m, k))
        }
        case k@Knowledge.Function(owner, name, source) => {
          val (s, e, ms) = format(source, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.Function(owner, name, s)))
        }
        case k@Knowledge.Procedure(owner, name, source) => {
          val (s, e, ms) = format(source, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.Procedure(owner, name, s)))
        }
        case k@Knowledge.View(owner, name, source) => {
          val (s, e, ms) = format(source, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.View(owner, name, s)))
        }
        case k@Knowledge.Synonym(_, name, targetOwner, targetName) => {
          if (name == targetName) ()
          else ddl(s"create or replace synonym $name for $targetName")
          
          (Mirage.ObjectType(targetOwner, targetName).wander(from).ref.toList, exp.updated(m, k))
        }
        case k@Knowledge.Package(_, name, _, _, _) => {
          ddl(s"create or replace package $name is end;")
          (Nil, exp.updated(m, k))
        }
        case k@Knowledge.Query(sql) => {
          val (s, e, ms) = format(sql, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.Query(s)))
        }
        case k@Knowledge.Block(sql) => {
          val (s, e, ms) = format(sql, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.Block(s)))
        }
        case k@Traveler.ChildObject(owner, parent, name) => {
          exp.collectFirst {
            case (m@Mirage.Package(_, name), k: Knowledge.Package) if name == parent => (m, k)
          } match {
            case Some((pm, pk)) => pk.hidden.get(name) match {
              case Some((s1, s2)) => {
                val (s1n, e1, ms1) = format(s1, exp)
                val (s2n, e2, ms2) = format(s2, e1)
                (ms1 ::: ms2 ::: pm :: Nil, exp.updated(pm, Knowledge.Package(pk.owner, pk.name, pk.children, pk.visible + (name -> (s1n, s2n)), pk.hidden - name))
                                               .updated(m, k))
              }
              case None => Mirage.ObjectType(owner, name).wander(from).ref match {
                case None => println(s"unable to resolve $m"); (Nil, exp)
                case Some(mirage) => (mirage :: Nil, exp.updated(m, k))
              }
            }
            case _ => Mirage.ObjectType(owner, parent).wander(from).ref match {
              case None => Mirage.ObjectType(owner, name).wander(from).ref match {
                case None => println(s"unable to resolve $m"); (Nil, exp)
                case Some(mirage) => (mirage :: m :: Nil, exp)
              }
              case Some(mirage) => (mirage :: m :: Nil, exp)
            }
          }
        }
        case k@Knowledge.Type(owner, name, source) => {
          val (s, e, ms) = format(source, exp)
          (ms ::: m :: Nil, exp.updated(m, Knowledge.Type(owner, name, s)))
        }
        case k@Knowledge.Sequence(owner, name, start, ordered) => {
          ddl(s"create sequence $name start with $start ${if (ordered) "order" else ""}")
          (Nil, exp.updated(m, k))
        }
      }
      case Some(k) => k match {
        case _: Knowledge.Table => (Nil, exp)
        case Knowledge.Function(owner, name, source) => {
          ddl(s"create or replace\r\n$source")
          val ms = fixDbError(owner, false, name)
          if (ms.isEmpty) (Nil, exp)
          else (ms ::: m :: Nil, exp)
        }
        case Knowledge.Procedure(owner, name, source) => {
          ddl(s"create or replace\r\n$source")
          val ms = fixDbError(owner, false, name)
          if (ms.isEmpty) (Nil, exp)
          else (ms ::: m :: Nil, exp)
        }
        case Knowledge.View(owner, name, source) => {
          try {
            ddl(s"""create or replace view $name as $source""")
            (Nil, exp)
          } catch {
            case e: SQLException => (fixSqlException(source, e, owner) ::: m :: Nil, exp)
          }
        }
        case Knowledge.Query(sql) => {
          val stat = to.createStatement()
          try {
            val rs = stat.executeQuery(sql)
            while (rs.next()) {
              ()
            }
            (Nil, exp)
          } catch {
            case e: SQLException => stat.close(); (fixSqlException(sql, e, to.getMetaData.getUserName.toUpperCase()) ::: m :: Nil, exp)
          }
        }
        case Knowledge.Block(sql) => {
          val stat = to.createStatement()
          try {
            val rs = stat.execute(sql)
            (Nil, exp)
          } catch {
            case e: SQLException => stat.close(); (fixPlSqlException(sql, e, to.getMetaData.getUserName.toUpperCase()) ::: m :: Nil, exp)
          }
        }
        case Knowledge.Package(owner, name, order, visible, hidden) => {
          val definition = order.map(visible.get(_)).flatten
          ddl((s"create or replace package $name IS\r\n" :: definition.map(_._1) ::: "end;" :: Nil).mkString("\r\n"))
          ddl((s"create or replace package body $name IS\r\n" :: definition.map(_._2) ::: "end;" :: Nil).mkString("\r\n"))
          val ms = fixDbError(owner, true, name)
          if (ms.isEmpty) (Nil, exp)
          else (ms ::: m :: Nil, exp)
        }
        case Knowledge.Type(owner, name, source) => {
          ddl(s"create or replace\r\n$source")
          val ms = fixDbError(owner, false, name)
          if (ms.isEmpty) (Nil, exp)
          else (ms ::: m :: Nil, exp)
        }
        case _ => (Nil, exp)
      }
    }
  }
  
  def visit(t: Knowledge.Table): Unit = {
    println(s"start migrating ${t.owner}.${t.name}")
    val batchSize = 1024 * 1024 * 8 // 1m
    val dml = {
      val colStr = t.columns.map(_.name).mkString(", ")
      val valStr = t.columns.map(_ => '?').mkString(", ")
      s"insert into ${t.name} ($colStr) values ($valStr)"
    }
    val stat = from.createStatement()
    val rs = stat.executeQuery(s"SELECT * FROM ${t.owner}.${t.name}")
    if (t.avgLen > 0) {
      rs.setFetchSize(batchSize / (t.avgLen * 8))
    } else ()
    val tableContent = to.prepareStatement(dml)
    @tailrec
    def prepare(columns: List[Knowledge.Column[_]], from: ResultSet, to: PreparedStatement, index: Int, bits: Long): Long = {
      if (columns.isEmpty) {
        to.addBatch()
        bits
      }
      else {
        val column = columns.head
        val inc = column.dataType.prepare(to, index, column.dataType.valueOf(from, index))
        prepare(columns.tail, from, to, index + 1, bits + inc)
      }
    }
    var cnt = 0l
    var row = 0
    while (rs.next()) {
      if (cnt > batchSize) {
        println(s"batching row: $row")
        tableContent.executeBatch()
        tableContent.clearBatch()
        cnt = 0
      } else {
        // do nothing
      }
      
      cnt += prepare(t.columns, rs, tableContent, 1, 0)
      row += 1
    }
    if (cnt > 0) {
      println(s"tail batching row: $row")
      tableContent.executeBatch()
      tableContent.clearBatch()
    }
    to.commit()
    tableContent.close()
    rs.close()
    stat.close()
    
    for (index <- t.indexes) {
      val cols = index.columns.mkString(",")
      val indexDef = to.createStatement()
      val uniqueness = if (index.unique) "UNIQUE" else ""
      val ddl = s"create $uniqueness index ${index.name} on ${t.name}(${cols})"
      println(s"executing: $ddl")
      indexDef.execute(ddl)
      indexDef.close()
    }
    
    println(s"done migrating ${t.owner}.${t.name}")
  }
  
  def ddl(sql: String): Unit = {
    val stat = to.createStatement()
    println(s"executing $sql")
    try {
      stat.execute(sql)
    } catch {
      case e: Throwable => stat.close; throw e
    }
    stat.close()
  }
  
  def analyse(owner: String, parent: Option[String], error: String, position: Int, source: String): Option[Mirage] = {
    val invalidIndentfierPattern = """"([a-zA-Z0-9_]+)": invalid identifier""".r
    val invalidChildIdentifierPattern = """"([a-zA-Z0-9_]+)"\."([a-zA-Z0-9_]+)": invalid identifier""".r
    val undeclaredIdentifierPattern = """identifier '([a-zA-Z0-9_]+)' must be declared""".r
    val undeclaredChildIdentifierPattern = """identifier '([a-zA-Z0-9_]+)\.([a-zA-Z0-9_]+)' must be declared""".r
    val undeclaredComponentPattern = """component '([a-zA-Z0-9_]+)' must be declared""".r
    val tableOrViewPattern = """table or view does not exist""".r
    val columnDisallowPattern = """column not allowed here""".r
    val noSeqPattern = """sequence does not exist""".r
    val nameEndPattern = """([a-zA-Z0-9_]+)\.$""".r
    val nameStartPattern = """^([a-zA-Z0-9_]+)""".r
    invalidChildIdentifierPattern.findFirstMatchIn(error) match {
      case Some(m) => Some(Traveler.ChildObject(owner, m.group(1).toUpperCase(), m.group(2).toUpperCase()))
      case _ => undeclaredIdentifierPattern.findFirstMatchIn(error) match {
        case Some(m) => parent match {
          case None => Mirage.ObjectType(owner, m.group(1).toUpperCase()).wander(from).ref
          case Some(p) => Some(Traveler.ChildObject(owner, p, m.group(1).toUpperCase()))
        }
        case _ => undeclaredComponentPattern.findFirstMatchIn(error) match {
          case Some(m) => nameEndPattern.findFirstMatchIn(source.substring(0, position - 1)) match {
            case Some(n) => Some(Traveler.ChildObject(owner, n.group(1).toUpperCase(), m.group(1).toUpperCase()))
            case _ => None
          }
          case _ => tableOrViewPattern.findFirstMatchIn(error) match {
            case Some(m) => nameStartPattern.findFirstMatchIn(source.substring(position - 1)) match {
                case Some(n) => Mirage.ObjectType(owner, n.group(1).toUpperCase()).wander(from).ref
                case _ => None
              }
            case _ => invalidIndentfierPattern.findFirstMatchIn(error) match {
              case Some(m) => parent match {
                case None => Mirage.ObjectType(owner, m.group(1).toUpperCase()).wander(from).ref
                case Some(p) => Some(Traveler.ChildObject(owner, p, m.group(1).toUpperCase()))
              }
              case _ => columnDisallowPattern.findFirstIn(error) match {
                case Some(m) => nameStartPattern.findFirstMatchIn(source.substring(position - 1)) match {
                  case Some(n) => parent match {
                    case None => Mirage.ObjectType(owner, n.group(1).toUpperCase()).wander(from).ref
                    case Some(p) => Some(Traveler.ChildObject(owner, p, n.group(1).toUpperCase()))
                  }
                  case None => None
                }
                case None => noSeqPattern.findFirstMatchIn(error) match {
                  case Some(m) => nameStartPattern.findFirstMatchIn(source.substring(position - 1)) match {
                    case None => None
                    case Some(n) => Mirage.ObjectType(owner, n.group(1).toUpperCase()).wander(from).ref
                  }
                  case _ => undeclaredChildIdentifierPattern.findFirstMatchIn(error) match {
                    case Some(m) => Some(Traveler.ChildObject(owner, m.group(1).toUpperCase(), m.group(2).toUpperCase()))
                    case _ => None
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  def fixSqlException(source: String, e: SQLException, owner: String): List[Mirage] = {
    val stat = to.prepareCall("""
      BEGIN
        EXECUTE IMMEDIATE 'BEGIN FOR i IN (' || 
? || ') LOOP null; END LOOP; END;';
      EXCEPTION WHEN OTHERS THEN
        ? := sqlerrm;
      END;""")
    stat.setString(1, source)
    stat.registerOutParameter(2, java.sql.Types.VARCHAR)
    stat.execute()
    val error = stat.getString(2)
    stat.close()
    val pattern = """line (\d+), column (\d+):\s+(.*)""".r
    val s = source.split('\n')
    pattern.findAllMatchIn(error).map(m => analyse(owner, None, m.group(3), m.group(2).toInt, s(m.group(1).toInt - 1))).toList.flatten
  }
  
  def fixPlSqlException(source: String, e: SQLException, owner: String): List[Mirage] = {
    val pattern = """line (\d+), column (\d+):""".r
    var ms: List[Mirage] = Nil
    var ex = e
    while (ex ne null) {
      val msg = ex.getMessage.split("\\n")
      pattern.findFirstMatchIn(msg(0)) match {
        case Some(m) => {
          ms = analyse(owner, None, msg(1), m.group(2).toInt, source.split("\\n")(m.group(1).toInt - 1)).toList ::: ms
        }
        case _ => ()
      }
      ex = ex.getNextException
    }
    ms.reverse
  }
  
  def fixDbError(owner: String, parent: Boolean, name: String): List[Mirage] = {
    val stat = to.prepareStatement("""
      SELECT e.text error, e.position, s.text
      FROM user_errors e, user_source s
      WHERE e.name = ?
        AND attribute = 'ERROR'
        AND s.name = ?
        AND e.line = s.line
      ORDER BY e.sequence""")
    stat.setString(1, name)
    stat.setString(2, name)
    val rs = stat.executeQuery()
    var ms: List[Mirage] = Nil
    while (rs.next()) {
      ms = analyse(owner, if (parent) Some(name) else None, rs.getString(1), rs.getInt(2), rs.getString(3)).toList ::: ms
    }
    rs.close()
    stat.close()
    ms
  }
  
  def format(sql: String, exp: Map[Mirage, Knowledge]): (String, Map[Mirage, Knowledge], List[Mirage]) = {
    val depPattern = """([a-zA-Z0-9_]+)\.([a-zA-Z0-9_]+)""".r
    def rec(rest: String, ms: List[Mirage], exp: Map[Mirage, Knowledge], terms: List[String]): (String, Map[Mirage, Knowledge], List[Mirage]) = {
      depPattern.findFirstMatchIn(rest) match {
        case Some(m) => {
          val typeMirage = Mirage.ObjectType(m.group(1).toUpperCase(), m.group(2).toUpperCase())
          exp.get(typeMirage) match {
            case Some(_) => rec(rest.substring(m.end), ms, exp, m.group(2) :: rest.substring(0, m.start) :: terms)
            case None => {
              typeMirage.wander(from) match {
                case k@Knowledge.ObjectType(_, _, Some(mirage)) => rec(rest.substring(m.end), mirage :: ms, exp.updated(typeMirage, k), m.group(2) :: rest.substring(0, m.start) :: terms)
                case _ => rec(rest.substring(m.end), ms, exp, rest.substring(0, m.end) :: terms)
              }
            }
          }
        }
        case _ => ((rest :: terms).reverse.mkString, exp, ms.reverse)
      }
    }
    rec(sql, Nil, exp, Nil)
  }
  
  def bypass(tableName: String): Traveler = new Traveler(from, to, bypass + tableName)
}

object Traveler {
  def of(from: Connection, to: Connection): Traveler = new Traveler(from, to, Set.empty)
  
  case class ChildObject(owner: String, parent: String, name: String) extends Mirage with Knowledge {
    override def wander(conn: Connection): ChildObject = this
  }
}