package exercises03.game

object Game {
  def parseState(input: String, number: Int): State =
    input match {
      case GameController.IGiveUp => GiveUp
      case _ =>
        input.toIntOption match {
          case Some(value) if value < number => NumberIsBigger
          case Some(value) if value > number => NumberIsSmaller
          case Some(_)                       => Guessed
          case None                          => WrongInput
        }
    }

  def action(state: State, number: Int): GameController => Unit =
    state match {
      case GiveUp          => _.giveUp(number)
      case WrongInput      => _.wrongInput()
      case NumberIsBigger  => _.numberIsBigger()
      case NumberIsSmaller => _.numberIsSmaller()
      case Guessed         => _.guessed()
    }

  def completed(state: State): Boolean = state == Guessed || state == GiveUp
}
