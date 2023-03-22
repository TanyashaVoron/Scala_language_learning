package exercises04

import exercises04.Machine.Input.{Coin, Turn}

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  @scala.annotation.tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) = {
    inputs match {
      case Nil => (machine, Nil)
      case _ =>
        if (machine.candies > 0) inputs.head match {
          case Coin if machine.locked  => run(Machine(locked = false, machine.candies, machine.coins + 1), inputs.tail)
          case Turn if !machine.locked => run(Machine(locked = true, machine.candies - 1, machine.coins), inputs.tail)
          case _                       => run(machine, inputs.tail)
        }
        else (machine, inputs)
    }
  }
}
