package exercises02.game
import scala.annotation.tailrec

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    */
  @tailrec
  final def play(number: Int): Unit = {
    controller.askNumber()
    val command = controller.nextLine()

    command match {
      case "I give up"               => controller.giveUp(number)
      case x if x == number.toString => controller.guessed()
      case _ =>
        val commandInt = command.toIntOption.getOrElse(-1)
        if (commandInt != -1) {
          if (commandInt < number) controller.numberIsBigger()
          if (commandInt > number) controller.numberIsSmaller()
        } else controller.wrongInput()
        play(number)
    }
  }
}
