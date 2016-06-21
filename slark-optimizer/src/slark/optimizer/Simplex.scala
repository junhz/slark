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
  case class StandardForm(a: Array[Array[Rational]], b: Array[Rational], c: Array[Rational], z: Rational, originalSize: Int) {
    def strict(ai: Int => Rational, bi: Rational) = {
      val nai = Array.fill(c.length + 1)(Rational.zero)
      var nbi = bi
      var col = 0
      while (col < c.length) {
        val factor = ai(col)
        if (c(col).isZero) {
          val row = View.Cols(a)(col).indexed.maxBy(_._2)._1
          var idx = 0
          while (idx < c.length) {
            nai(idx) -= factor * a(row)(idx)
            idx += 1
          }
          nai(col) = Rational.zero
          nbi -= factor * b(row)
        } else nai(col) += factor
        col += 1
      }
      nai(c.length) = Rational.one//Rational(BigInteger.ONE, nai.foldLeft(BigInteger.ONE)((o, i) => o.multiply(i.denominator).divide(i.denominator.gcd(o))))
      StandardForm(a.map(_ :+ Rational.zero) :+ nai, b :+ nbi, c :+ Rational.zero, z, originalSize)
    }
    
    override def toString = {
      def name(idx: Int) = if (idx < originalSize) s"x$idx" else s"s${idx - originalSize}"
      val max = s"max $z ${View.Array(c).indexed.map({ case (i, ci) =>
        if (ci.isZero) "" else { if(ci.isPositive) s"+ $ci${name(i)}" else s"- ${ci.negate}${name(i)}" }
      }).mkString(" ")}"
      val subjectTo = if (b.length > 0) {
        val aStr = View.Rows(a).map(ai => {
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
        View.Rows(a).indexed.map {
          case (i, ai) => ai.indexed.map {
            case (j, aij) => {
              if (j == 0) new String(Array.fill(ajMaxStrLen(j) - aStr(i)(j)._2.length())(' ')) + aStr(i)(j)._2
              else aStr(i)(j)._1 + new String(Array.fill(ajMaxStrLen(j) - aStr(i)(j)._2.length() + 1)(' ')) + aStr(i)(j)._2
            }
          }.mkString(" ") + s" <= ${b(i)}"
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
      println(s"pivot at ($row, $col)")
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
      show(matrix)
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
  
  def withObject(c: Array[Rational]): StandardForm = {
    StandardForm(Array[Array[Rational]](), Array[Rational](), c, Rational.zero, c.length)
  }
  
  def format(problem: LinearProgram): StandardForm = {
    import LinearProgram._
    val size = problem.obj.coefficients.length
    val cLen = size + problem.consts.count(!_.isEquality)
    val c = problem.obj match {
      case Max(coe) => coe ++: Array.fill(cLen - size)(Rational.zero)
      case Min(coe) => coe.map(_.negate) ++: Array.fill(cLen - size)(Rational.zero)
    }
    val b = problem.consts.map {
      case _ ≤ bi => bi
      case _ `=` bi => bi
      case _ ≥ bi => bi.negate
    }.toArray
    var slackIdx = 0
    val a = problem.consts.map {
      case ai ≤ _ => {
        val nai = Array.fill(cLen)(Rational.zero)
        var i = 0
        while (i < size) {
          nai(i) = ai(i)
          i += 1
        }
        nai(slackIdx + size) = Rational.one
        slackIdx += 1
        nai
      }
      case ai `=` _ => {
        val nai = Array.fill(cLen)(Rational.zero)
        var i = 0
        while (i < size) {
          nai(i) = ai(i)
          i += 1
        }
        nai
      }
      case ai ≥ _ => {
        val nai = Array.fill(cLen)(Rational.zero)
        var i = 0
        while (i < size) {
          nai(i) = ai(i).negate
          i += 1
        }
        nai(slackIdx + size) = Rational.one
        slackIdx += 1
        nai
      }
    }.toArray
    StandardForm(a, b, c, Rational.zero, size)
  }
}