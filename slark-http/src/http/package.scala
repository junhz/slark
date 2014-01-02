package slark

import logger._

package object http extends Log {
  private[http] lazy val log = newLogger
}