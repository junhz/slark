package slark.optimizer

import scala.io.StdIn
import slark.optimizer.Simplex.SolveResult

/**
 * @author a554114
 */
object TestInteger {
  
  val splitPattern = """ +""".r

  val vectorPattern = """(?i) *(max|min)((?: +-?\d+)+) *""".r
  
  val subjectPattern = """ *(-?\d+) *(>=|=|<=) *((?: +-?\d+)+) *""".r

  val cmdPattern = """(?i)(exit|cut|B&B|B&C)""".r
  
  def main(args: Array[String]) {

    var problem: Option[LinearProgram] = None
    var stop = false

    while (!stop) {
      val line = StdIn.readLine()
      line match {
        case vectorPattern(name, coe) => {
          val kind = name.toUpperCase() match {
            case "MAX" => LinearProgram.Max
            case "MIN" => LinearProgram.Min
          }
          problem = Some(LinearProgram(kind(splitPattern.split(coe).tail.map(s => Rational.fromInt(s.toInt))), Vector.empty))
          println(problem.get)
        }
        case subjectPattern(b, r, a) => {
          val coe = splitPattern.split(a).tail.map(s => Rational.fromInt(s.toInt))
          val const = Rational.fromInt(b.toInt)
          problem match {
            case None => ()
            case Some(p) => {
              val kind = r match {
                case ">=" => LinearProgram.≤
                case "<=" => LinearProgram.≥
                case "="  => LinearProgram.`=`
              }
              problem = Some(p.withConstraint(kind(i => if (i < coe.length) coe(i) else Rational.zero, const)))
            }
          }
          println(problem.get)
        }
        case cmdPattern(cmd) => {
          cmd.toUpperCase() match {
            case "EXIT" => stop = true
            case "B&B" => problem match {
              case Some(p) => println(BranchAndBound().solve(Simplex.format(p)))
              case _ => ()
            }
            case "CUT" => problem match {
              case Some(p) => println(Gomory.solve(Simplex.format(p)))
              case _ => ()
            }
            case "B&C" => problem match {
              case Some(p) => println(BranchAndBound().and(Gomory).solve(Simplex.format(p)))
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