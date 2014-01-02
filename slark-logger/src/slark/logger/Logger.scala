package slark
package logger

import scala.collection.immutable.Iterable
import java.util.Date

/**
 * log logic
 */
sealed trait Logger {
  final def apply(record: Logger.Record) {
    @tailrec
    def rec(configs: Iterable[Logger.Config]) {
      if (configs.isEmpty) {}
      else {
        val (level, formatter, writer) = configs.head
        if (record.level reaches level) {
          writer.write(formatter.format(record.date, record.level, record.source, record.message))
        } else {}
        rec(configs.tail)
      }
    }
    rec(configs)
  }

  final def apply(record: Logger.Record, ex: Throwable) {
    val exLines = ex.fullMessage
    @tailrec
    def rec(configs: Iterable[Logger.Config]) {
      if (configs.isEmpty) {}
      else {
        val (level, formatter, writer) = configs.head
        if (record.level reaches level) {
          writer.write(formatter.format(record.date, record.level, record.source, record.message))
          exLines.foreach(writer.write(_))
        } else {}
        rec(configs.tail)
      }
    }
    rec(configs)
  }

  def configs: Iterable[Logger.Config]
}

object Logger {
  type Config = (Level, Formatter, Writer)
  final class Record(val source: String, val level: Level, context: StringContext, args: List[Any]) {
    val date = new Date
    lazy val message = context.s(args: _*)
  }

  val silent = new Logger {
    override def configs = Iterable.empty
  }

  def apply(configSeq: Config*): Logger = if (configSeq.isEmpty) silent else new Logger {
    override def configs = configSeq.toList
  }
}