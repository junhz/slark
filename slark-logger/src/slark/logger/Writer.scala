package slark.logger

trait Writer {
  /**
   * write formated message
   */
  def write(message: String): Unit
}

object Writer {
  // TODO: write monoid

  val console = new Writer {
    override def write(message: String) {
      println(message)
    }
  }

  // TODO: file writer
}