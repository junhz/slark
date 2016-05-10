package slark.mirage.db

/**
 * @author a554114
 */
trait Constraint
object Constraint {
  case object Unique extends Constraint
  case object Primary extends Constraint
}