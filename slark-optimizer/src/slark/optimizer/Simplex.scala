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
  case class StandardForm(a: View.Indexed[View.Indexed[Rational]], 
                          b: View.Indexed[Rational], 
                          c: View.Indexed[Rational], 
                          z: Rational, 
                          varSize: Int,
                          slackSize: Int,
                          constraintSize: Int) {
    def strict(ai: View.Indexed[Rational], bi: Rational) = {
      val len = varSize + slackSize
      val nai = ai.fill(len + 1, Rational.zero).updated(len, Rational.one)
      StandardForm(a.map(_ :+ Rational.zero) :+ nai, b :+ bi, c :+ Rational.zero, z, varSize, slackSize + 1, constraintSize + 1)
    }
    
    def strict(constraints: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val len = varSize + slackSize
      val consts = constraints.toList.toArray(Array[(View.Indexed[Rational], Rational)]())
      val size = consts.length
      val na = View.Range(0, size).map(i => consts(i)._1.fill(len + size, Rational.zero).updated(len + i, Rational.one))
      StandardForm(a.map(_ .fill(len + size, Rational.zero)) :++ na, b :++ View.Array(consts).map(_._2), c.fill(len + size, Rational.zero), z, varSize, slackSize + size, constraintSize + size)
    }
    
    def format(ai: View.Indexed[Rational], bi: Rational): (View.Indexed[Rational], Rational) = {
      val len = varSize + slackSize
      val nai = ai.fill(len, Rational.zero).toArray
      var nbi = bi
      val bVars = basicVars()
      bVars.foreach {
        case (col, bi, ai) => {
          val factor = nai(col)
          var idx = 0
          while (idx < len) {
            nai(idx) -= factor * ai(idx)
            idx += 1
          }
          nbi -= factor * bi
        }
      }
      (View.Array(nai), nbi)
    }
    
    def basicVars(): View.Travesal[(Int, Rational, View.Indexed[Rational])] = {
      val colLen = varSize + slackSize
      val rowLen = constraintSize
      View.Range(0, colLen).some(c(_).isZero)
                             .some(col => (View.Range(0, rowLen).count(row => !a(row)(col).isZero)) == 1)
                             .map(col => (View.Range(0, rowLen).first(row => !a(row)(col).isZero), col))
                             .map {
        case (row, col) => {
          val factor = a(row)(col)
          (col, b(row) / factor, a(row).map(_ / factor))
        }
      }
    }
    
    override def toString = {
      def name(idx: Int) = if (idx < varSize) s"x$idx" else s"s${idx - varSize}"
      val max = s"max $z ${c.indexed.map({ case (i, ci) =>
        if (ci.isZero) "" else { if(ci.isPositive) s"+ $ci${name(i)}" else s"- ${ci.negate}${name(i)}" }
      }).mkString(" ")}"
      val subjectTo = if (constraintSize > 0) {
        val aStr = a.map(ai => {
          var first = true
          ai.indexed.map {
            case (j, aij) => {
              if (aij.isZero) (' ', "")
              else {
                if (first) {
                  first = false
                  if (j == 0) (' ', s"$aij${name(j)}")
                  else if (aij.isPositive) (' ', s"$aij${name(j)}") else ('-', s"${aij.negate}${name(j)}")
                } else if (aij.isPositive) ('+', s"$aij${name(j)}") else ('-', s"${aij.negate}${name(j)}")
              }
            }
          }.toArray
        }).toArray
        val ajMaxStrLen = View.Cols(aStr).map(_.map(_._2.length).max).toArray
        a.indexed.map {
          case (i, ai) => ai.indexed.map {
            case (j, aij) => {
              if (j == 0) new String(Array.fill(ajMaxStrLen(j) - aStr(i)(j)._2.length())(' ')) + aStr(i)(j)._2
              else aStr(i)(j)._1 + new String(Array.fill(ajMaxStrLen(j) - aStr(i)(j)._2.length() + 1)(' ')) + aStr(i)(j)._2
            }
          }.mkString(" ") + s" = ${b(i)}"
        }.toArray
      } else Array[Array[String]]()
      
      (max +: subjectTo).mkString("\r\n")
    }
  }

  sealed trait SolveResult

  case class Optimized(problem: StandardForm) extends SolveResult {
    override def toString = s"Optimized\r\n$problem"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  trait Phase {

    def solve(problem: StandardForm): SolveResult

    final def pivot(matrix: Array[Array[Rational]], row: Int, col: Int): Unit = {
      //println(s"pivot at ($row, $col)")
      var r = 0
      while (r < matrix.length) {
        val vector = matrix(r)
        var c = 0
        val factor = vector(col)
        while (c < vector.length) {
          if (r == row) {
            vector(c) /= factor
          } else {
            vector(c) -= matrix(row)(c) * factor / matrix(row)(col)
          }
          c += 1
        }
        r += 1
      }
      //show(matrix)
    }

    final def show(matrix: Array[Array[Rational]]): Unit = {
      val textLen = View.Cols(matrix).indexed.map({ case (col, arr) => arr.map(_.toString().length()).max }).toArray
      View.Rows(matrix).foreach(arr => println(arr.map(_.toString()).indexed.map({
        case (col, s) => new String(Array.fill(textLen(col) - s.length)(' ')) + s
      }).mkString(" ")))
    }
  }
  
  trait Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int)
  }
  
  def format(problem: LinearProgram): StandardForm = {
    import LinearProgram._
    val varSize = problem.coefficients.length
    val constraintSize = problem.consts.size
    val slackIdx = new Array[Int](constraintSize)
    var slackSize = 0
    View.Range(0, constraintSize).foreach(i => {
      val const = problem.consts(i)
      if (const.relation.isEquality) slackIdx(i) = -1 
      else {
        slackIdx(i) = slackSize
        slackSize += 1
      }
    })
    val cLen = varSize + slackSize
    val c = problem.obj match {
      case Max => problem.coefficients.fill(cLen, Rational.zero)
      case Min => problem.coefficients.map(_.negate).fill(cLen, Rational.zero)
    }
    val b = View.Vector(problem.consts).map {
      case Constraint(_, <=, bi) => bi
      case Constraint(_, `=`, bi) => bi
      case Constraint(_, >=, bi) => bi.negate
      case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
    }
    
    val a = View.Range(0, constraintSize).map { i => {
      val const = problem.consts(i)
      const match {
        case Constraint(ai, <=, _) => ai.fill(cLen, Rational.zero).updated(slackIdx(i) + varSize, Rational.one)
        case Constraint(ai, `=`, _) => ai.fill(cLen, Rational.zero)
        case Constraint(ai, >=, _) => ai.map(_.negate).fill(cLen, Rational.zero).updated(slackIdx(i) + varSize, Rational.one)
        case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
      }
    }}
    StandardForm(a, b, c, Rational.zero, varSize, slackSize, a.length)
  }
}