package exercises03.game

sealed trait State

//сдаться
case object GiveUp          extends State

//некорректный ввод
case object WrongInput      extends State
case object NumberIsBigger  extends State
case object NumberIsSmaller extends State
//угадал
case object Guessed         extends State
