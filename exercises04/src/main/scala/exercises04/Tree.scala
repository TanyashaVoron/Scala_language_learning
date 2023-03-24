package exercises04

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(v)             => f(v)
  }
  def size[A](t: Tree[A]): Int  = fold(t)(_ => 1)(1 + _ + _)
  def max(t: Tree[Int]): Int    = fold(t)((v: Int) => v)(_ max _)
  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((l: Int, r: Int) => 1 + (l max r))
  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)((v: A) => Leaf(f(v)): Tree[B])(Branch(_, _))
}
