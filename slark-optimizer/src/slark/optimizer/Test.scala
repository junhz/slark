package slark.optimizer

import scala.io.StdIn
import slark.optimizer.Simplex.SolveResult

/**
 * @author a554114
 */
object Test {
  
  val splitPattern = """ +""".r

  val vectorPattern = """ *(-?\d+)((?: +-?\d+)+) *""".r

  val cmdPattern = """(?i)(linear|discard|exit|quit|cut|B&B|B&C)""".r
  
  def main(args: Array[String]) {

    var problem: Option[Simplex.StandardForm] = None
    var simplexMethod: Simplex = Primal
    var stop = false

    while (!stop) {
      val line = StdIn.readLine()
      line match {
        case vectorPattern(head, tail) => {
          val constraint = splitPattern.split(tail).tail
          problem = problem match {
            case Some(p) => Some(p.withConstraint(i => if (i < constraint.length) Rational.fromInt(constraint(i).toInt) else Rational.zero, Rational.fromInt(head.toInt)))
            case _ => Some(Simplex.withObject((head +: constraint).map(s => Rational.fromInt(s.toInt))))
          }
          println(problem.get)
        }
        case cmdPattern(cmd) => {
          cmd.toUpperCase() match {
            case "EXIT" | "QUIT" => stop = true
            case "DISCARD" => problem = None; simplexMethod = Primal
            case "LINEAR"  => problem match {
              case Some(p) => problem = {
                val r = simplexMethod.solve(p)
                println(r)
                r match {
                  case Simplex.Optimized(p) => Some(p)
                  case _                    => problem
                }
              }
              case _ => ()
              simplexMethod = problem match {
                case Some(_) => Dual
                case _ => Primal
              }
            }
            case "B&B" => problem match {
              case Some(p) => println(BranchAndBound.solve(p))
              case _ => ()
            }
            case "CUT" => problem match {
              case Some(p) => println(Gomory.solve(p))
              case _ => ()
            }
            case "B&C" => problem match {
              case Some(p) => println(BranchAndCut.solve(p))
              case _ => ()
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }
  }
}