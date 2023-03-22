package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def caseOperation(left: Expr[T], right: Expr[T], operation: Char): Result[T] = {
    (calculate(left), calculate(right)) match {
      case (Success(v1), Success(v2)) =>
        operation match {
          case '+'                => Success(v1 + v2)
          case '-'                => Success(v1 - v2)
          case '*'                => Success(v1 * v2)
          case '/' if !isZero(v2) => Success(v1 / v2)
          case _                  => DivisionByZero
        }
      case _ => DivisionByZero
    }
  }

  def calculate(expr: Expr[T]): Result[T] = expr match {

    case Val(v) => Success(v)

    case Mul(left, right) => caseOperation(left, right, '*')

    case Plus(left, right) => caseOperation(left, right, '+')

    case Minus(left, right) => caseOperation(left, right, '-')

    case Div(left, right) => caseOperation(left, right, '/')

    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(v) if iff(v) => calculate(left)
        case Success(_)           => calculate(right)
        case _                    => DivisionByZero
      }
  }
}
