package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = if (funcs.isDefinedAt(-1)) Some(funcs())
}
