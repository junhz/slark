package slark.optimizer

import scala.io.StdIn

/**
 * @author a554114
 */
object TestLinear {
  
  val splitPattern = """ +""".r

  val vectorPattern = """(?i) *(max|min)((?: +-?\d+)+) *""".r
  
  val subjectPattern = """ *(-?\d+) *(>=|=|<=) *((?: +-?\d+)+) *""".r

  val cmdPattern = """(?i)(solve|exit)""".r
  
  def main(args: Array[String]) {

    var problem: Option[Either[Simplex.StandardForm, LinearProgram]] = None
    var stop = false

    while (!stop) {
      val line = StdIn.readLine()
      line match {
        case vectorPattern(name, coe) => {
          val kind = name.toUpperCase() match {
            case "MAX" => LinearProgram.Max
            case "MIN" => LinearProgram.Min
          }
          problem = Some(Right(LinearProgram(kind(splitPattern.split(coe).tail.map(s => Rational.fromInt(s.toInt))), Vector.empty)))
          println(problem.get.merge)
        }
        case subjectPattern(b, r, a) => {
          val coe = splitPattern.split(a).tail.map(s => Rational.fromInt(s.toInt))
          val const = Rational.fromInt(b.toInt)
          problem match {
            case None => ()
            case Some(e) => e match {
              case Left(p) => {
                Some(Left(p.strict(i => if (i < coe.length) coe(i) else Rational.zero, const)))
              }
              case Right(p) => {
                val kind = r match {
                  case ">=" => LinearProgram.≤
                  case "<=" => LinearProgram.≥
                  case "="  => LinearProgram.`=`
                }
                problem = Some(Right(p.withConstraint(kind(i => if (i < coe.length) coe(i) else Rational.zero, const))))
              }
            }
          }
          println(problem.get.merge)
        }
        case cmdPattern(cmd) => {
          cmd.toUpperCase() match {
            case "EXIT" => stop = true
            case "SOLVE"  => problem match {
              case Some(e) => {
                val np = e match {
                  case Left(p)  => Dual.solve(p)
                  case Right(p) => Primal.solve(Simplex.format(p))
                }
                problem = np match {
                  case Simplex.Optimized(st) => Some(Left(st))
                  case _ => problem
                }
                println(np)
              }
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