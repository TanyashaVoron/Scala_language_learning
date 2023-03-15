package exercises03

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int], sumElementsList: Int = 0): Int = {
    list match {
      case Nil => sumElementsList
      case Cons(head,tail) => sum(Cons(head, tail), sumElementsList)
    }
  }

  def reverse[A](list: MyList[A], convertList: MyList[A] = new MyList[A] {}): MyList[A] = {
    list match {
      case Cons(head, tail) => reverse(Cons(head, convertList), tail)
      case Nil => convertList
    }
  }
}
