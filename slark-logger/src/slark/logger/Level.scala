package slark.logger

sealed trait Level {
  /**
   * if message level reaches logger level, do logging
   */
  def reaches(that: Level): Boolean
}

object Level {
  val info = new Level {
    override def toString = "info"

    /**
     * info only reaches info level
     */
    override def reaches(that: Level) = this eq that
  }
  val warn = new Level {
    override def toString = "warn"
    /**
     * warning reaches any log level except error
     */
    override def reaches(that: Level) = that != error
  }
  val error = new Level {
    override def toString = "error"
    /**
     * always log error
     */
    override def reaches(that: Level) = true
  }
}