package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = {
    @tailrec
    def sumList(list: MyList[Int], sumElementsList: Int): Int =
      list match {
        case Cons(head, tail) => sumList(tail, sumElementsList + head)
        case Nil              => sumElementsList
      }
    sumList(list, 0)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def rebuild(curr: MyList[A], convertList: MyList[A]): MyList[A] = curr match {
      case Nil              => convertList
      case Cons(head, tail) => rebuild(tail, Cons(head, convertList))
    }
    rebuild(list, Nil)
  }
}
