package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = expr match {

    case Val(v) => Success(v)

    case Mul(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(v1), Success(v2)) => Success(v1 * v2)
        case _ => DivisionByZero
      }

    case Plus(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(v1), Success(v2)) => Success(v1 + v2)
        case _ => DivisionByZero
      }

    case Minus(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(v1), Success(v2)) => Success(v1 - v2)
        case _ => DivisionByZero
      }

    case Div(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(v1), Success(v2)) if !isZero(v2) => Success(v1 / v2)
        case _ => DivisionByZero
      }

    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(v) if iff(v) => calculate(left)
        case Success(_) => calculate(right)
        case _ => DivisionByZero
      }
  }
}
