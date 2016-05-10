package slark.mirage

import java.sql.Connection

/**
 * @author a554114
 */
trait Mirage {
  def wander(conn: Connection): Knowledge
}

object Mirage {
  
  case class Table(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Table = new Knowledge.Table(owner, name, loadColumns(conn), loadIndexes(conn), loadAvgLen(conn))

    def loadColumns(conn: Connection): List[Knowledge.Column[_]] = {
      val stat = conn.prepareStatement(s"""
        SELECT column_name, 
               data_type, 
               data_length, 
               data_precision, 
               data_scale, 
               nullable,
               char_length,
               char_used
        FROM dba_tab_cols
        WHERE hidden_column = 'NO'
          AND virtual_column = 'NO'
          AND owner = ?
          AND table_name = ?
        ORDER BY column_id""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(200)
      var list: List[Knowledge.Column[_]] = Nil
      while (rs.next()) {
        list = Knowledge.Column(rs.getString(1), db.DataType(rs.getString(2), 
                                                             rs.getInt(3),
                                                             rs.getInt(4),
                                                             rs.getInt(5),
                                                             rs.getInt(7),
                                                             rs.getString(8)), "Y" == rs.getString(6)) :: list
      }
      rs.close()
      stat.close()
      list.reverse
    }

    def loadIndexes(conn: Connection): List[Knowledge.Index] = {
      val stat = conn.prepareStatement(s"""
        SELECT index_name, 
               uniqueness
        FROM dba_indexes
        WHERE owner = ?
          AND table_name = ?
          AND index_type = 'NORMAL'
          AND visibility = 'VISIBLE'
          AND dropped = 'NO'
          AND generated = 'N'
          AND temporary = 'N'""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      var list: List[Knowledge.Index] = Nil
      while (rs.next()) {
        val indexName = rs.getString(1)
        val unique = rs.getString(2)
        list = Knowledge.Index(indexName, "UNIQUE" == unique, loadIndexColumns(indexName, conn)) :: list
      }
      rs.close()
      stat.close()
      list.reverse
    }

    def loadIndexColumns(name: String, conn: Connection): List[String] = {
      val stat = conn.prepareStatement(s"""
        SELECT column_name
        FROM dba_ind_columns
        WHERE index_name = ?
        ORDER BY column_position""")
      stat.setString(1, name)
      val rs = stat.executeQuery()
      var list: List[String] = Nil
      while (rs.next()) {
        list = rs.getString(1) :: list
      }
      rs.close()
      stat.close()
      list.reverse
    }
    
    def loadAvgLen(conn: Connection): Int = {
      val stat = conn.prepareStatement(s"""
        SELECT avg_row_len
        FROM dba_tab_statistics
        WHERE owner = ?
          AND table_name = ?""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.next()
      val l = rs.getInt(1)
      rs.close()
      stat.close()
      l
    }
  }
  
  case class Function(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Function = {
      val stat = conn.prepareStatement(s"""
        SELECT text
        FROM dba_source
        WHERE owner = ?
          AND name = ?
          AND type = 'FUNCTION'
        ORDER BY line""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(1000)
      var lines: Vector[String] = Vector.empty
      while (rs.next()) {
        lines = lines :+ rs.getString(1)
      }
      rs.close()
      stat.close()
      Knowledge.Function(owner, name, lines.mkString)
    }
  }
  
  case class Procedure(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Procedure = {
      val stat = conn.prepareStatement(s"""
        SELECT text
        FROM dba_source
        WHERE owner = ?
          AND name = ?
          AND type = 'PROCEDURE'
        ORDER BY line""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(1000)
      var lines: Vector[String] = Vector.empty
      while (rs.next()) {
        lines = lines :+ rs.getString(1)
      }
      rs.close()
      stat.close()
      Knowledge.Procedure(owner, name, lines.mkString)
    }
  }
  
  case class View(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.View = {
      val stat = conn.prepareStatement("""
        SELECT text_length, 
               text
        FROM dba_views
        WHERE owner = ?
          AND view_name = ?""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.next()
      val r = rs.getCharacterStream(2)
      val buf = new Array[Char](rs.getInt(1))
      r.read(buf)
      r.close()
      rs.close()
      stat.close()
      Knowledge.View(owner, name, new String(buf))
    }
  }
  
  case class Synonym(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Synonym = {
      val stat = conn.prepareStatement("""
        SELECT table_owner,
               table_name
        FROM dba_synonyms
        WHERE owner = ?
          AND synonym_name = ?""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.next()
      val targetOwner = rs.getString(1)
      val targetName = rs.getString(2)
      rs.close()
      stat.close()
      Knowledge.Synonym(owner, name, targetOwner, targetName)
    }
  }
  
  case class Package(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Package = {
      val declare = loadDeclares(conn)
      val definition = loadDefinitions(conn)
      var hidden: Map[String, (String, String)] = Map.empty
      declare.foreach(t => hidden = hidden + (t._1 -> (t._2, definition.getOrElse(t._1, ""))))
      Knowledge.Package(owner, name, declare.map(_._1), Map.empty, hidden)
    }
    
    def simplify(source: String): (String, Array[String]) = {
      val strPattern = """'([^']|(''))*'"""
      val singleLineCommentPattern = """\-\-.*"""
      val multiLinesCommentHeadPattern = """\/\*"""
      val multiLinesCommentTailPattern = """\*\/""".r
      val p = s"($strPattern)|($singleLineCommentPattern)|($multiLinesCommentHeadPattern)".r
      def rec(rest: String, literals: List[String], terms: List[String], count: Int): (String, Array[String]) = {
        p.findFirstMatchIn(rest) match {
          case Some(m) if m.matched.startsWith("'") => rec(rest.substring(m.end), rest.substring(m.start + 1, m.end - 1) :: literals, s"'$count'" :: rest.substring(0, m.start) :: terms, count + 1)
          case Some(m) if m.matched.startsWith("--") => rec(rest.substring(m.end), rest.substring(m.start + 2, m.end) :: literals, s"--$count" :: rest.substring(0, m.start) :: terms, count + 1)
          case Some(m) if m.matched.startsWith("/*") => {
            multiLinesCommentTailPattern.findFirstMatchIn(rest.substring(m.end)) match {
              case Some(n) => rec(rest.substring(m.end + n.end), rest.substring(m.end, m.end + n.start) :: literals, s"/*$count*/" :: rest.substring(0, m.start) :: terms, count + 1)
              case _ => throw new IllegalArgumentException("unclosed comment")
            }
          }
          case _ => ((rest :: terms).reverse.mkString, literals.reverse.toArray)
        }
      }
      rec(source, Nil, Nil, 0)
    }
    
    def complexify(source: String, literals: Array[String]): String = {
      val strPattern = """'\d+'"""
      val singleLineCommentPattern = """\-\-\d+"""
      val multiLinesCommentPattern = """\/\*\d+\*\/"""
      val p = s"($strPattern)|($singleLineCommentPattern)|($multiLinesCommentPattern)".r
      def rec(rest: String, terms: List[String]): String = {
        p.findFirstMatchIn(rest) match {
          case Some(m) if m.matched.startsWith("'") => rec(rest.substring(m.end), s"'${literals(rest.substring(m.start + 1, m.end - 1).toInt)}'" :: rest.substring(0, m.start) :: terms)
          case Some(m) if m.matched.startsWith("--") => rec(rest.substring(m.end), s"--${literals(rest.substring(m.start + 2, m.end).toInt)}" :: rest.substring(0, m.start) :: terms)
          case Some(m) if m.matched.startsWith("/*") => rec(rest.substring(m.end), s"/*${literals(rest.substring(m.start + 2, m.end - 2).toInt)}*/" :: rest.substring(0, m.start) :: terms)
          case _ => (rest :: terms).reverse.mkString
        }
      }
      rec(source, Nil)
    }

    def loadDeclares(conn: Connection): List[(String, String)] = {
      val nonVariablePattern = """(?i)(type|procedure|subtype|function|cursor)\s+([a-zA-Z0-9_]+)[^;]+;""".r
      val variablePattern = """(?i)([a-zA-Z0-9_]+)\s+[^;]+;""".r
      val stat = conn.prepareStatement("""
      SELECT text
      FROM dba_source
      WHERE owner = ?
        AND name = ?
        AND type = 'PACKAGE'
      ORDER BY line""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(1000)
      var lines: Vector[String] = Vector.empty
      while (rs.next()) {
        lines = lines :+ rs.getString(1)
      }
      rs.close()
      stat.close()
      def rec(source: String, items: List[(String, String)]): List[(String, String)] = {
        nonVariablePattern.findFirstMatchIn(source) match {
          case Some(m) => rec(source.substring(m.end), (m.group(2).toUpperCase(), m.matched) :: items)
          case _ => variablePattern.findFirstMatchIn(source) match {
            case Some(m) => rec(source.substring(m.end), (m.group(1).toUpperCase(), m.matched) :: items)
            case _       => items.reverse
          }
        }
      }
      val (s, a) = simplify(lines.mkString)
      rec(s, Nil).map(s => (s._1, complexify(s._2, a)))
    }

    def loadDefinitions(conn: Connection): Map[String, String] = {
      val namePattern = """\s*([a-zA-Z0-9_]+)""".r
      val keyWords = """(?i)[^a-zA-Z0-9_](procedure|function|begin|end|if|case|loop)[^a-zA-Z0-9_]""".r
      val stat = conn.prepareStatement("""
      SELECT text
      FROM dba_source
      WHERE owner = ?
        AND name = ?
        AND type = 'PACKAGE BODY'
      ORDER BY line""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(1000)
      var lines: Vector[String] = Vector.empty
      while (rs.next()) {
        lines = lines :+ rs.getString(1)
      }
      rs.close()
      stat.close()
      def rec(code: String, terms: List[String], text: List[String], items: Map[String, String]): Map[String, String] = {
        keyWords.findFirstMatchIn(code) match {
          case Some(m) => m.group(1).toUpperCase() match {
            case "PROCEDURE" | "FUNCTION" => namePattern.findFirstMatchIn(code.substring(m.end)) match {
              case Some(n) => terms match {
                case Nil => rec(code.substring(m.end + n.end), n.group(1) :: terms, code.substring(m.start, m.end + n.end) :: text, items)
                case _   => rec(code.substring(m.end + n.end), n.group(1) :: terms, code.substring(0, m.end + n.end) :: text, items)
              }
              case _ => throw new IllegalArgumentException("incomplete procedure or function signature")
            }
            case s @ ("IF" | "CASE" | "LOOP" | "BEGIN") => rec(code.substring(m.end), s :: terms, code.substring(0, m.end) :: text, items)
            case "END" => terms match {
              case Nil => items
              case t :: ts => t match {
                case s @ ("LOOP" | "IF") => namePattern.findFirstMatchIn(code.substring(m.end)) match {
                  case Some(n) if n.group(1).toUpperCase() == s => rec(code.substring(m.end + n.end), ts, code.substring(0, m.end + n.end) :: text, items)
                  case _                                        => throw new IllegalArgumentException(s"unclosed $s")
                }
                case "CASE" => namePattern.findFirstMatchIn(code.substring(m.end)) match {
                  case Some(n) if n.group(1).toUpperCase() == "CASE" => rec(code.substring(m.end + n.end), ts, code.substring(0, m.end + n.end) :: text, items)
                  case _ => rec(code.substring(m.end), ts, code.substring(0, m.end) :: text, items)
                }
                case "BEGIN" => ts match {
                  case Nil => throw new IllegalArgumentException("anonymous block")
                  case t :: ts => t match {
                    case s @ ("IF" | "CASE" | "LOOP" | "BEGIN") => rec(code.substring(m.end), t :: ts, code.substring(0, m.end) :: text, items)
                    case _ => namePattern.findFirstMatchIn(code.substring(m.end)) match {
                      case Some(n) if n.group(1).toUpperCase() == t => ts match {
                        case Nil => rec(code.substring(m.end + n.end), Nil, Nil, items + (t.toUpperCase() -> (code.substring(0, m.end + n.end) :: text).reverse.mkString))
                        case _   => rec(code.substring(m.end + n.end), ts, code.substring(0, m.end + n.end) :: text, items)
                      }
                      case _ => ts match {
                        case Nil => rec(code.substring(m.end), Nil, Nil, items + (t.toUpperCase() -> (code.substring(0, m.end(1)) :: text).reverse.mkString))
                        case _   => rec(code.substring(m.end), ts, code.substring(0, m.end) :: text, items)
                      }
                    }
                  }
                }
                case _ => throw new IllegalArgumentException(s"incompelete procedure or function definition")
              }
            }
          }
          case _ => throw new IllegalArgumentException("unclosed package")
        }
      }
      val (s, a) = simplify(lines.mkString)
      rec(s, Nil, Nil, Map.empty).mapValues(s => complexify(s, a) + ";")
    }
  }
  
  case class Query(sql: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Query = Knowledge.Query(sql)
  }
  
  case class ObjectType(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.ObjectType = {
      val stat = conn.prepareStatement("""
          SELECT object_type
          FROM dba_objects
          WHERE owner = ?
            AND object_name = ?
          ORDER BY object_type""")
        stat.setString(1, owner)
        stat.setString(2, name)
        val rs = stat.executeQuery()
        if (rs.next()) {
          val m = rs.getString(1) match {
            case "TABLE" => Mirage.Table(owner, name)
            case "FUNCTION" => Mirage.Function(owner, name)
            case "PROCEDURE" => Mirage.Procedure(owner, name)
            case "VIEW" => Mirage.View(owner, name)
            case "SYNONYM" => Mirage.Synonym(owner, name)
            case "PACKAGE" => Mirage.Package(owner, name)
            case "TYPE" => Mirage.Type(owner, name)
            case "SEQUENCE" => Mirage.Sequence(owner, name)
          }
          rs.close
          stat.close()
          Knowledge.ObjectType(owner, name, Some(m))
        } else {
          rs.close()
          stat.close()
          Knowledge.ObjectType(owner, name, None)
        }
    }
  }
  
  case class `Type`(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Type = {
      val stat = conn.prepareStatement("""
        SELECT text
        FROM dba_source
        WHERE owner = ?
          AND name = ?
          AND type = 'TYPE'
        ORDER BY line""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.setFetchSize(1000)
      var lines: Vector[String] = Vector.empty
      while (rs.next()) {
        lines = lines :+ rs.getString(1)
      }
      rs.close()
      stat.close()
      Knowledge.Type(owner, name, lines.mkString.replaceFirst(s"""(?i)type\\s+"$name"\\s+""", s"""TYPE "$name" force """))
    }
  }
  
  case class Sequence(owner: String, name: String) extends Mirage {
    def wander(conn: Connection): Knowledge.Sequence = {
      val stat = conn.prepareStatement("""
        SELECT last_number, order_flag
        FROM dba_sequences
        WHERE sequence_owner = ?
          AND sequence_name = ?""")
      stat.setString(1, owner)
      stat.setString(2, name)
      val rs = stat.executeQuery()
      rs.next()
      val start = rs.getLong(1)
      val ordered = "Y" == rs.getString(2)
      rs.close()
      stat.close()
      Knowledge.Sequence(owner, name, start, ordered)
    }
  }
}