package codes.quine.leanprops

/** `InspectConfig` haves a configuration for `inspect` method.
  *   - `size` means a number of showing size of elements of '''infinite''' collection, or cases of function.
  *   - `singleLine` determines using newlines for inspecting function.
  *   - `inner` is a configuration which is used for inspecting nested values.
  * There are some pre-configured values: `EightLines` and `FourCases`.
  */
case class InspectConfig(size: Int,
                         singleLine: Boolean,
                         inner: () => InspectConfig)

object InspectConfig {

  /** Shows eight cases with newlines on function inspection.
    * It is the default configuration of `inspect`.
    */
  val EightLines: InspectConfig = InspectConfig(8, false, () => FourCases)

  /** Shows four cases with `;` on function inspection.
    * It is used for inner functions on `inspect`.
    */
  val FourCases: InspectConfig = InspectConfig(4, true, () => FourCases)
}

/** `WithInspectConfig` is a 'Reader' monad specified for InspectConfig`. */
case class WithInspectConfig[A](run: InspectConfig => A) extends AnyVal {
  self =>
  def map[B](f: A => B): WithInspectConfig[B] =
    WithInspectConfig(c => f(self.run(c)))

  def flatMap[B](f: A => WithInspectConfig[B]): WithInspectConfig[B] =
    WithInspectConfig(c => f(self.run(c)).run(c))
}

object WithInspectConfig {
  import Inspectable._
  import Testable._

  def readConfig: WithInspectConfig[InspectConfig] = WithInspectConfig(c => c)

  def pure[A](v: A): WithInspectConfig[A] = WithInspectConfig(_ => v)

  def inner[A](m: WithInspectConfig[A]): WithInspectConfig[A] =
    WithInspectConfig(c => m.run(c.inner()))

  def inspect1[A: Inspectable](a: A)(
      f: String => String): WithInspectConfig[String] = inner(inspect(a).map(f))

  def inspect2[A: Inspectable, B: Inspectable](a: A, b: B)(
      f: (String, String) => String): WithInspectConfig[String] =
    inner(for { s1 <- inspect(a); s2 <- inspect(b) } yield f(s1, s2))

  def inspect3[A: Inspectable, B: Inspectable, C: Inspectable](
      a: A,
      b: B,
      c: C)(f: (String, String, String) => String): WithInspectConfig[String] =
    inner(
      for { s1 <- inspect(a); s2 <- inspect(b); s3 <- inspect(c) } yield
        f(s1, s2, s3))

  def inspect4[A: Inspectable, B: Inspectable, C: Inspectable, D: Inspectable](
      a: A,
      b: B,
      c: C,
      d: D)(f: (String, String, String, String) => String)
    : WithInspectConfig[String] =
    inner(for {
      s1 <- inspect(a); s2 <- inspect(b); s3 <- inspect(c); s4 <- inspect(d)
    } yield f(s1, s2, s3, s4))

  def inspect5[A: Inspectable,
               B: Inspectable,
               C: Inspectable,
               D: Inspectable,
               E: Inspectable](a: A, b: B, c: C, d: D, e: E)(
      f: (String, String, String, String, String) => String)
    : WithInspectConfig[String] =
    inner(for {
      s1 <- inspect(a); s2 <- inspect(b); s3 <- inspect(c); s4 <- inspect(d);
      s5 <- inspect(e)
    } yield f(s1, s2, s3, s4, s5))

  def inspects[A: Inspectable](xs: Seq[A]): WithInspectConfig[Seq[String]] =
    inner(WithInspectConfig(c => xs.map(x => inspect(x).run(c))))

  def sequence[A](xs: Seq[WithInspectConfig[A]]): WithInspectConfig[Seq[A]] =
    WithInspectConfig(c => xs.map(_.run(c)))

  def sequenceBinding[A](
      b: Binding[WithInspectConfig[A]]): WithInspectConfig[Binding[A]] =
    b match {
      case Binding(vs, r) =>
        WithInspectConfig(
          c => Binding(vs.map(_.map(_.run(c))), r.map(_.run(c))))
    }

  def sequenceBindings[A](bs: Seq[Binding[WithInspectConfig[A]]])
    : WithInspectConfig[Seq[Binding[A]]] = sequence(bs.map(sequenceBinding))

  def sequenceResult[A](
      r: Result[WithInspectConfig[A]]): WithInspectConfig[Result[A]] = r match {
    case Result(vs, ok) =>
      WithInspectConfig(c => Result(vs.map(_.map(_.run(c))), ok))
  }

  def sequenceResults[A](rs: Seq[Result[WithInspectConfig[A]]])
    : WithInspectConfig[Seq[Result[A]]] = sequence(rs.map(sequenceResult))

  def cons0[A](name: String): WithInspectConfig[String] = pure(name)

  def cons1[A: Inspectable](name: String, a: A): WithInspectConfig[String] =
    inspect1(a)(s => s"$name($s)")

  def cons2[A: Inspectable, B: Inspectable](name: String,
                                            a: A,
                                            b: B): WithInspectConfig[String] =
    inspect2(a, b)((s1, s2) => s"$name($s1, $s2)")

  def cons3[A: Inspectable, B: Inspectable, C: Inspectable](
      name: String,
      a: A,
      b: B,
      c: C): WithInspectConfig[String] =
    inspect3(a, b, c)((s1, s2, s3) => s"$name($s1, $s2, $s3)")

  def cons4[A: Inspectable, B: Inspectable, C: Inspectable, D: Inspectable](
      name: String,
      a: A,
      b: B,
      c: C,
      d: D): WithInspectConfig[String] =
    inspect4(a, b, c, d)((s1, s2, s3, s4) => s"$name($s1, $s2, $s3, $s4)")

  def cons5[A: Inspectable,
            B: Inspectable,
            C: Inspectable,
            D: Inspectable,
            E: Inspectable](name: String,
                            a: A,
                            b: B,
                            c: C,
                            d: D,
                            e: E): WithInspectConfig[String] =
    inspect5(a, b, c, d, e)((s1, s2, s3, s4, s5) =>
      s"$name($s1, $s2, $s3, $s4, $s5)")
}
