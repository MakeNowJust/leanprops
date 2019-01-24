package leanprops

private[leanprops] final case class TableFunction1[A, R](map: Map[A, R],
                                                         default: R)
    extends (A => R) {
  def apply(a: A): R = map.getOrElse(a, default)
}
private[leanprops] final case class TableFunction2[A, B, R](map: Map[(A, B), R],
                                                            default: R)
    extends ((A, B) => R) {
  def apply(a: A, b: B): R = map.getOrElse((a, b), default)
}

private[leanprops] final case class TableFunction3[A, B, C, R](
    map: Map[(A, B, C), R],
    default: R)
    extends ((A, B, C) => R) {
  def apply(a: A, b: B, c: C): R = map.getOrElse((a, b, c), default)
}

private[leanprops] final case class TableFunction4[A, B, C, D, R](
    map: Map[(A, B, C, D), R],
    default: R)
    extends ((A, B, C, D) => R) {
  def apply(a: A, b: B, c: C, d: D): R = map.getOrElse((a, b, c, d), default)
}

private[leanprops] final case class TableFunction5[A, B, C, D, E, R](
    map: Map[(A, B, C, D, E), R],
    default: R)
    extends ((A, B, C, D, E) => R) {
  def apply(a: A, b: B, c: C, d: D, e: E): R =
    map.getOrElse((a, b, c, d, e), default)
}
