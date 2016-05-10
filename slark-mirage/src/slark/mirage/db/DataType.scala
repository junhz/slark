package slark.mirage.db

import java.sql.ResultSet
import java.sql.PreparedStatement
import java.sql.Clob
import java.nio.CharBuffer
import java.sql.Blob
import java.sql.Timestamp
import java.nio.channels.Channels
import java.nio.ByteBuffer

/**
 * @author a554114
 */
trait DataType[T] {
  def valueOf(rs: ResultSet, index: Int): T
  def prepare(stat: PreparedStatement, index: Int, value: T): Long
  def toString: String
}

object DataType {
  trait CharDataType extends DataType[String] {
    def valueOf(rs: ResultSet, index: Int) = rs.getString(index)
    def prepare(stat: PreparedStatement, index: Int, value: String) = {
      stat.setString(index, value)
      if (value eq null) 0 else value.getBytes.length
    }
  }
  case class CHAR(size: Int, unit: String) extends CharDataType {
    override def toString = s"CHAR($size $unit)"
  }
  case class VARCHAR2(size: Int, unit: String) extends CharDataType {
    override def toString = s"VARCHAR2($size $unit)"
  }
  trait NumberDataType extends DataType[java.math.BigDecimal] {
    def valueOf(rs: ResultSet, index: Int) = rs.getBigDecimal(index)
    def prepare(stat: PreparedStatement, index: Int, value: java.math.BigDecimal) = {
      stat.setBigDecimal(index, value)
      22
    }
  }
  case class NUMBER(precision: Int, scale: Int) extends NumberDataType {
    override def toString = {
      if (precision > 0) {
        if (scale > 0) s"NUMBER($precision, $scale)" else s"NUMBER($precision)"
      }
      else "NUMBER"
    }
  }
  case class FLOAT(precision: Int) extends NumberDataType {
    override def toString = if (precision > 0) s"FLOAT($precision)" else "FLOAT"
  }
  case class TimestampDataType(override val toString: String) extends DataType[Timestamp] {
    def valueOf(rs: ResultSet, index: Int) = rs.getTimestamp(index)
    def prepare(stat: PreparedStatement, index: Int, value: Timestamp) = {
      stat.setTimestamp(index, value)
      20 // 10 ~ 20 for data type
    }
  }
  case object CLOB extends DataType[Clob] {
    def valueOf(rs: ResultSet, index: Int) = rs.getClob(index)
    def prepare(stat: PreparedStatement, index: Int, value: Clob) = {
      if (value eq null) {
        stat.setNull(index, java.sql.Types.CLOB)
        0
      } else {
        val t = stat.getConnection.createClob()
        val w = t.setCharacterStream(0)
        val r = value.getCharacterStream
        val buf = CharBuffer.allocate(1024)
        while (r.read(buf) > 0) {
          buf.flip()
          w.write(buf.array(), 0, buf.position())
          buf.clear()
        }
        w.close()
        r.close()
        stat.setClob(index, t)
        value.length()
      }
    }
  }
  case object BLOB extends DataType[Blob] {
    def valueOf(rs: ResultSet, index: Int) = rs.getBlob(index)
    def prepare(stat: PreparedStatement, index: Int, value: Blob) = {
      if (value eq null) {
        stat.setNull(index, java.sql.Types.BLOB)
        0
      } else {
        val t = stat.getConnection.createBlob()
        val w = Channels.newChannel(t.setBinaryStream(0))
        val r = Channels.newChannel(value.getBinaryStream)
        val buf = ByteBuffer.allocate(1024)
        while (r.read(buf) > 0) {
          w.write(buf)
        }
        w.close()
        r.close()
        stat.setBlob(index, t)
        value.length()
      }
    }
  }
  def apply(typeName: String, length: Int, precision: Int, scale: Int, charLen: Int, charUsed: String): DataType[_] = {
    typeName match {
      case "CHAR" => charUsed match {
        case "B" => CHAR(length, "BYTE")
        case _   => CHAR(charLen, "CHAR")
      }
      case "VARCHAR2" => charUsed match {
        case "B" => VARCHAR2(length, "BYTE")
        case _   => VARCHAR2(charLen, "CHAR")
      }
      case "NUMBER"                       => NUMBER(precision, scale)
      case "FLOAT"                        => FLOAT(precision)
      case "DATE"                         => TimestampDataType("DATE")
      case t if t.startsWith("TIMESTAMP") => TimestampDataType(t)
      case "BLOB"                         => BLOB
      case "CLOB"                         => CLOB
    }
  }
}