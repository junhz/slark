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
      StandardForm(a :+ nai, b :+ bi, c, z, nbv :+ SlackVar(n), bv)
    }
    
    def newSlacks(variables: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val vars = variables.toArray
      val na = View.Array(vars).map(_._1.fill(m, Rational.zero))
      StandardForm(a :++ na, b :++ View.Array(vars).map(_._2), c, z, nbv :++ View.Range(0, vars.length).map(ord => SlackVar(ord + n)), bv)
    }
    
    def subjectTo(constraint: LinearProgram.Constraint): StandardForm = {
      import LinearProgram._
      val ncoe = constraint.coefficients.fill(m + n, Rational.zero)
      val nai = View.Range(0, m).map(i => bv(i) match {
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
        case `<=` => newSlack(View.Array(nai), nbi)
        case `>=` => newSlack(View.Array(nai).map(_.negate), nbi.negate)
        case `=` => newSlack(View.Array(nai), nbi).newSlack(View.Array(nai).map(_.negate), nbi.negate)
        case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
      }
    }
    // TODO: use lhs rhs instead
    override def toString = {
      case class Tapped(pre: String, post: String) {
        def fill(len: Int) = new String((View.OfString(pre).fill(len - post.length(), ' ') :++ View.OfString(post)).toArray)
        def length = pre.length() + post.length()
      }
      def tapped(coe: View.Indexed[Rational], col: Int) = {
        val r = coe(col)
        r.signum() match {
          case 0 => Tapped("   ", "")
          case 1 => Tapped(" - ", s"$r${bv(col)}")
          case -1 => Tapped(" + ", s"${r.negate}${bv(col)}")
        }
      }
      val tappedView = (Tapped("max", "") +: View.Range(0, n).map(row => Tapped(nbv(row).toString(), s" = "))) +:
                       (z +: b).map(r => Tapped("", r.toString())) +: 
                       View.Range(0, m).map(col => tapped(c, col) +: View.Range(0, n).map(row => tapped(a(row), col)))
      val t = tappedView.map(_.toArray).toArray
      val len = View.Rows(t).map(col => col.map(_.length).max).toArray
      View.Range(0, n + 1).map(row => View.Range(0, m + 2).map(col => t(col)(row).fill(len(col))).mkString).mkString("\r\n")
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
      val pr = View.Array(matrix(row)).toArray
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
      //show(matrix)
    }

    final def show(matrix: Array[Array[Rational]]): Unit = {
      val textLen = View.Cols(matrix, matrix(0).length).map(arr => arr.map(_.toString().length()).max).toArray
      val s = View.Rows(matrix).map(arr => View.Range(0, textLen.length).map(col => {
        val s = arr(col).toString()
        new String(Array.fill(textLen(col) - s.length)(' ')) + s
      }).mkString(" "))
      println(s.mkString("\r\n"))
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
    val constraintSize = consts.length
    val surplusIdx = new Array[Int](constraintSize)
    var surplusSize = 0
    val surplus = new Array[Variable](constraintSize)
    val nonBasic = new Array[Variable](constraintSize)
    View.Range(0, constraintSize).foreach(i => consts(i) match {
      case Constraint(_, <=, _)   => nonBasic(i) = SlackVar(i)
      case Constraint(ai, `=`, _) => nonBasic(i) = ArtifactVar(i)
      case Constraint(ai, >=, _) => {
        nonBasic(i) = ArtifactVar(i)
        surplus(i) = SurplusVar(i)
        surplusIdx(i) = surplusSize
        surplusSize += 1
      }
      case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
    })
    val basic = View.Range(0, decideSize).map(ord => DecideVar(ord): Variable) :++ (View.Array(surplus).range(0, surplusSize))
    val basicSize = decideSize + surplusSize
    val c = problem.obj match {
      case Max => problem.coefficients.fill(basicSize, Rational.zero)
      case Min => problem.coefficients.map(_.negate).fill(basicSize, Rational.zero)
    }
    val b = View.Vector(consts).map(_.constant)
    
    val a = View.Range(0, constraintSize).map { i => {
      val const = consts(i)
      const match {
        case Constraint(ai, >=, _) => ai.fill(basicSize, Rational.zero).update(surplusIdx(i) + decideSize, Rational.one.negate)
        case Constraint(ai, _, _) => ai.fill(basicSize, Rational.zero)
      }
    }}
    StandardForm(a, b, c, Rational.zero, View.Array(nonBasic), basic)
  }
}