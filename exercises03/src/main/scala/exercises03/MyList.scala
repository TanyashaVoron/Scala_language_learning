package exercises03

import scala.annotation.tailrec

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

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def fold(curr: MyList[A], convertList: MyList[A]): MyList[A] = curr match {
      case Nil              => convertList
      case Cons(head, tail) => fold(tail, Cons(head, convertList))
    }
    fold(list, Nil)
  }
}
