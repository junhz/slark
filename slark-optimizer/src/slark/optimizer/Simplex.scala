package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
trait Simplex {
  import Simplex._
  def solve(problem: StandardForm): SolveResult
}

object Simplex {
  trait Variable {
    def ord: Int
  }
  case class DecideVar(val ord: Int) extends Variable {
    override def toString = s"x$ord"
  }
  case class SlackVar(val ord: Int) extends Variable {
    override def toString = s"s+$ord"
  }
  case class SurplusVar(val ord: Int) extends Variable {
    override def toString = s"s-$ord"
  }
  case class ArtifactVar(val ord: Int) extends Variable {
    override def toString = s"a$ord"
  }
  case class StandardForm(a: View.Indexed[View.Indexed[Rational]], 
                          b: View.Indexed[Rational], 
                          c: View.Indexed[Rational], 
                          z: Rational,
                          nbv: View.Indexed[Variable],
                          bv: View.Indexed[Variable]) {
    val m = bv.length
    val n = nbv.length
    def newSlack(ai: View.Indexed[Rational], bi: Rational) = {
      val nai = ai.fill(m, Rational.zero)
      StandardForm(nai +: a, bi +: b, c, z, SlackVar(n) +: nbv , bv)
    }
    
    def newSlacks(variables: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val vars = variables.toArray
      val na = View.OfArray(vars).map(_._1.fill(m, Rational.zero))
      StandardForm(a :++ na, b :++ View.OfArray(vars).map(_._2), c, z, nbv :++ View.OfRange(0, vars.length).map(ord => SlackVar(ord + n)), bv)
    }
    
    def subjectTo(constraint: LinearProgram.Constraint): StandardForm = {
      import LinearProgram._
      val ncoe = constraint.coefficients.fill(m + n, Rational.zero)
      val nai = bv.map(bvi => bvi match {
        case DecideVar(ord) => ncoe(ord)
        case _              => Rational.zero
      }).toArray
      var nbi = constraint.constant
      var row = 0
      while (row < n) {
        nbv(row) match {
          case DecideVar(ord) => {
            val factor = ncoe(ord)
            var col = 0
            while (col < m) {
              nai(col) -= factor * a(row)(col)
              col += 1
            }
            nbi -= factor * b(row)
          }
          case _ => ()
        }
        row += 1
      }
      constraint.relation match {
        case `<=` => newSlack(View.OfArray(nai), nbi)
        case `>=` => newSlack(View.OfArray(nai).map(_.negate), nbi.negate)
        case `=` => newSlack(View.OfArray(nai), nbi).newSlack(View.OfArray(nai).map(_.negate), nbi.negate)
        case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
      }
    }
    
    override def toString = {
      def trOf(ri: View.Indexed[Rational]): View.Indexed[Table.Cell] = {
        View.OfRange(0, m).map(col => {
          val r = ri(col)
          r.signum() match {
            case 0  => Table.Cell("  ", "")
            case 1  => Table.Cell("- ", s"$r${bv(col)}")
            case -1 => Table.Cell("+ ", s"${r.negate}${bv(col)}")
          }
        })
      }
      val table = (Table.Cell("max", "") +: Table.Cell("", z.toString()) +: trOf(c)) +:
                  View.OfRange(0, n).map(row => Table.Cell(nbv(row).toString(), " =") +: Table.Cell("", b(row).toString()) +: trOf(a(row)))
      Table.mkString(table, " ", "\r\n")
    }
  }

  sealed trait SolveResult

  case class Optimized(solution: StandardForm) extends SolveResult {
    override def toString = s"Optimized\r\n$solution"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  trait Phase {

    def solve(problem: StandardForm): SolveResult

    final def pivot(matrix: Array[Array[Rational]], row: Int, col: Int): Unit = {
      //println(s"pivot at ($row, $col)")
      var r = 0
      val pr = View.OfArray(matrix(row)).toArray
      while (r < matrix.length) {
        val vector = matrix(r)
        var c = 0
        val factor = vector(col)
        while (c < vector.length) {
          if (r == row) {
            if (c == col) vector(c) = Rational.one / factor
            else vector(c) /= factor
          } else {
            if (c == col) vector(c) = -factor / pr(col)
            else vector(c) -= pr(c) * factor / pr(col)
          }
          c += 1
        }
        r += 1
      }
      // show(matrix)
    }

    final def show(matrix: Array[Array[Rational]]): Unit = {
      val table = View.OfArray(matrix).map(View.OfArray(_).map(r => Table.Cell("", r.toString)))
      println(Table.mkString(table, " ", "\r\n"))
    }
  }
  
  trait Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int)
  }
  
  def format(problem: LinearProgram): StandardForm = {
    import LinearProgram._
    val decideSize = problem.varSize
    val consts = problem.consts map {
      const => if (const.constant.isNegative) const.negate() else const
    }
    val n = consts.length
    val surplus = View.OfRange(0, n).some(consts(_).relation == `>=`).toArray
    val m = decideSize + surplus.length;
    
    val nonBasic = View.OfRange(0, n).map(i => consts(i).relation match {
      case `<=` => SlackVar(i): Variable
      case _ => ArtifactVar(i): Variable
    })
    
    val basic = View.OfRange(0, decideSize).map(ord => DecideVar(ord): Variable) :++ View.OfArray(surplus).map(ord => SurplusVar(ord): Variable)

    val c = (problem.obj match {
      case Max => problem.coefficients
      case Min => problem.coefficients.map(_.negate)
    }).fill(m, Rational.zero)
    
    val b = View.OfVector(consts).map(_.constant)
    
    val a = View.OfRange(0, n).map { row => 
      consts(row).coefficients.fill(decideSize, Rational.zero) :++ View.OfArray(surplus).map(col => if (col == row) Rational.one.negate else Rational.zero)
    }
    StandardForm(a, b, c, Rational.zero, nonBasic, basic)
  }
}