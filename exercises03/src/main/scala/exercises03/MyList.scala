package exercises03

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int], sumElementsList: Int = 0): Int = {
    list match {
      case Cons(head, tail) => head + sum(tail, sumElementsList)
      case Nil              => 0
    }
  }

  def reverse[A](list: MyList[A], convertList: MyList[A] = Nil): Any = {
    list match {
      case Cons(head, tail) => reverse(tail, Cons(head, convertList))
      case Nil              => convertList
    }
  }
}
