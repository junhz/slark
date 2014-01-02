package slark.logger

import java.util.Date

trait Formatter {
  def format(date: Date, level: Level, source: String, message: String): String
}

object Formatter {
  val default = new Formatter {
    override def format(date: Date, level: Level, source: String, message: String) =
      s"""[$date] $level $source: $message"""
  }
}