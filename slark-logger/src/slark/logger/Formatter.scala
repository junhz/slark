package slark
package logger

trait Formatter {
  def format(date: DateTime, level: Level, source: String, message: String): String
}

object Formatter {
  val default = new Formatter {
    override def format(date: DateTime, level: Level, source: String, message: String) =
      s"""[$date] $level $source: $message"""
  }
}