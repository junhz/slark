package slark.logger

trait Writer {
  /**
   * write formated message
   */
  def write(message: String): Unit
}

object Writer {

  val console = new Writer {
    override def write(message: String) {
      println(message)
    }
  }

}