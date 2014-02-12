package slark
package logger

import scala.collection.immutable.Iterable

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
    @tailrec
    def rec(configs: Iterable[Logger.Config]) {
      if (configs.isEmpty) {}
      else {
        val (level, formatter, writer) = configs.head
        if (record.level reaches level) {
          writer.write(formatter.format(record.date, record.level, record.source, record.message))
          logException(ex, writer)
        } else {}
        rec(configs.tail)
      }
    }
    rec(configs)
  }

  private[this] def logException(e: Throwable, writer: Writer): Unit = {
    @tailrec
    def rec(throwns: List[Thrown[_]]): Unit = {
      if (throwns.isEmpty) {}
      else {
        writer.write(s"Caused by: ${throwns.head}\r\n")
        rec(throwns.tail)
      }
    }
    val throwns = Thrown.wrap(e)
    writer.write(s"${throwns.head}\r\n")
    rec(throwns.tail)
  }

  def configs: Iterable[Logger.Config]
}

object Logger {
  type Config = (Level, Formatter, Writer)
  final class Record(val source: String, val level: Level, context: StringContext, args: List[Any]) {
    val date = DateTime.since1970(System.currentTimeMillis())
    lazy val message = context.s(args: _*)
  }

  val silent = new Logger {
    override def configs = Iterable.empty
  }

  def apply(configSeq: Config*): Logger = if (configSeq.isEmpty) silent else new Logger {
    override def configs = configSeq.toList
  }
}