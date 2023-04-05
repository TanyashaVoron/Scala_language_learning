package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def flatMap[EE >: A, AA](f: B => Either[EE, AA]): Either[EE, AA] = this match {
      case Left(x)  => Left(x)
      case Right(x) => f(x)
    }
    def map[AA](f: B => AA): Either[A, AA] = this match {
      case Left(x)  => Left(x)
      case Right(x) => Right(f(x))
    }
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = (this, other) match {
      case (Right(x), _)      => Right(x)
      case (Left(x), Left(_)) => Left(x)
      case _                  => other
    }
    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] =
      this.flatMap(x => other.map(y => f(x, y)))
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(a) => Right(a)
      case None    => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      list.foldLeft(Right(List.empty[B]): Either[E, List[B]]) { (acc, a) =>
        acc.flatMap(bs => f(a).map(b => bs :+ b))
      }
    }

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
      traverse(list)(identity)
  }
}
