package state
import typeclasses.Monad
import scala.annotation.tailrec

object StatefulRandom {
  import scala.util.Random
  println(Random.nextDouble)
  println(Random.nextInt)
  println(Random.nextInt)
}

object StatelessRandom {
  trait Random {
    def nextInt: (Int, Random)
  }
  case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Int, Random) = {
      val newSeed = 13 * seed + 41
      val int     = (newSeed >>> 3).toInt
      (int, SimpleRandom(newSeed))
    }
  }

  val rnd: SimpleRandom     = SimpleRandom(12)
  val (firstInt, nextRnd1)  = rnd.nextInt
  val (secondInt, nextRnd2) = nextRnd1.nextInt
  val (thirdInt, nextRnd3)  = nextRnd2.nextInt

  def pair(rnd: Random): ((Int, Int), Random) = {
    val (i, nextRng1) = rnd.nextInt
    val (j, nextRng2) = nextRng1.nextInt
    ((i, j), nextRng2)
  }
  def nonNegativeInt(rnd: Random): (Int, Random) = {
    val (i, nextRng) = rnd.nextInt
    (math.abs(i), nextRng)
  }
  def double(rnd: Random): (Double, Random) = {
    val (i, nextRng) = nonNegativeInt(rnd)
    (i / Int.MaxValue.toDouble, nextRng)
  }
}

object BetterStatelessRandom {

  import typeclasses.Monad.syntax._
  import StatelessRandom.Random

  case class RandomState[A](run: Random => (A, Random))

  object RandomState {
    implicit val monad: Monad[RandomState] = new Monad[RandomState] {
      override def pure[A](a: A): RandomState[A] = RandomState(rng => (a, rng))

      override def map[A, B](fa: RandomState[A])(f: A => B): RandomState[B] =
        RandomState(rng => {
          val (a, nextRng) = fa.run(rng)
          (f(a), nextRng)
        })

      override def flatMap[A, B](fa: RandomState[A])(f: A => RandomState[B]): RandomState[B] =
        RandomState(rng => {
          val (a, nextRng) = fa.run(rng)
          f(a).run(nextRng)
        })
    }
  }

  val nextInt: RandomState[Int] = RandomState(_.nextInt)

  val nonNegativeInt: RandomState[Int] = nextInt.map(value => if (value < 0) -value else value)

  val pair: RandomState[(Int, Int)] =
    for {
      i <- nonNegativeInt
      j <- nonNegativeInt
    } yield (i, j)

  val double: RandomState[Double] =
    nonNegativeInt.map(_ / Int.MaxValue.toDouble)

  val randomList: RandomState[List[Int]] = {
    for {
      len    <- nonNegativeInt
      values <- sequence(List.fill(len)(nextInt))
    } yield values
  }
  def sequence[A](xs: List[RandomState[A]]): RandomState[List[A]] =
    xs.foldLeft(List[A]().pure[RandomState])((lst, value) =>
      for {
        list <- lst
        v    <- value
      } yield list.prepended(v)
    )
}
