package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def caseOperation(left: Expr[T], right: Expr[T], f: (T, T) => T): Result[T] = {
    (calculate(left), calculate(right)) match {
      case (Success(v1), Success(v2)) => Success(f(v1, v2))
      case _                          => DivisionByZero
    }
  }

  def calculate(expr: Expr[T]): Result[T] = expr match {

    case Val(v) => Success(v)

    case Mul(left, right) => caseOperation(left, right, (v1, v2) => v1 * v2)

    case Plus(left, right) => caseOperation(left, right, (v1, v2) => v1 + v2)

    case Minus(left, right) => caseOperation(left, right, (v1, v2) => v1 - v2)

    case Div(left, right) =>
      calculate(right) match {
        case Success(v) if !isZero(v) => caseOperation(left, right, (v1, v2) => v1 / v2)
        case _                        => DivisionByZero
      }

    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(v) if iff(v) => calculate(left)
        case Success(_)           => calculate(right)
        case _                    => DivisionByZero
      }
  }
}
