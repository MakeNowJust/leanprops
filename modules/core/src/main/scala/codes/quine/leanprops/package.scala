package codes
package quine

package object leanprops {

  /** Inspects `Inspectable` value, or better `toString`.
    * See `Inspectable` documentation for more details.
    *
    * {{{
    * scala> inspect("hello world")
    * res0: String = "hello world"
    *
    * scala> inspect("escape\nsequence\tis\balso\fescaped")
    * res1: String = "escape\nsequence\tis\balso\fescaped"
    *
    * scala> inspect[Int => String] { case 1 => "You can inspect"; case 2 => "function also" }
    * res2: String =
    * (x => x match {
    *   case 1 => "You can inspect"
    *   case 2 => "function also"
    * })
    * }}}
    */
  def inspect[A: Inspectable](a: A): String =
    Inspectable.inspect(a).run(InspectConfig.EightLines)

  /** Lists all counter-examples for a number of tests to a property.
    *
    * {{{
    * scala> counterExamples(10) { (n: Int) => n % 3 == 0 }
    * res0: Seq[Seq[Seq[String]]] = Stream(List(List(1)), ?)
    * }}}
    */
  def counterExamples[A: Testable](n: Int)(p: A): Seq[Seq[Seq[String]]] =
    WithInspectConfig
      .liftResults(Testable.results[A](p).take(n).filterNot(_.isOk))
      .run(InspectConfig.FourCases)
      .map(_.values)

  /**  Up to a number of tests to a property, returns `Some` the first counter-example or `None` if there is none.
    *
    * {{{
    * scala> counterExample(10) { (n: Int) => n % 3 == 0 }
    * res0: Option[Seq[Seq[String]]] = Some(List(List(1)))
    *
    * scala> counterExample(10) { (n: Int) => n == n }
    * res1: Option[Seq[Seq[String]]] = None
    * }}}
    */
  def counterExample[A: Testable](n: Int)(p: A): Option[Seq[Seq[String]]] =
    counterExamples(n)(p).headOption

  /** Lists all witnesses for a number of tests to a property.
    *
    * {{{
    * scala> witnesses(10) { (n: Int) => n % 3 == 0 }
    * res0: Seq[Seq[Seq[String]]] = Stream(List(List(0)), ?)
    * }}}
    */
  def witnesses[A: Testable](n: Int)(p: A): Seq[Seq[Seq[String]]] =
    WithInspectConfig
      .liftResults(Testable.results[A](p).take(n).filter(_.isOk))
      .run(InspectConfig.FourCases)
      .map(_.values)

  /**  Up to a number of tests to a property, returns '''Some''' the first witness or '''None''' if there is none.
    *
    * {{{
    * scala> witness(10) { (n: Int) => n % 3 == 0 }
    * res0: Option[Seq[Seq[String]]] = Some(List(List(0)))
    *
    * scala> witness(10) { (n: Int) => n != n }
    * res1: Option[Seq[Seq[String]]] = None
    * }}}
    */
  def witness[A: Testable](n: Int)(p: A): Option[Seq[Seq[String]]] =
    witnesses(n)(p).headOption

  /** Does a property hold up to a number of test values?
    *
    * {{{
    * scala> holds(10) { (n: Int) => n == n }
    * res0: Boolean = true
    *
    * scala> holds(10) { (n: Int) => n != n }
    * res1: Boolean = false
    * }}}
    */
  def holds[A: Testable](n: Int)(p: A): Boolean =
    Testable.results[A](p).take(n).forall(_.isOk)

  /** Does a property __fail__ for a number of test values?
    *
    * {{{
    * scala> fails(10) { (n: Int) => n == n }
    * res0: Boolean = false
    *
    * scala> fails(10) { (n: Int) => n != n }
    * res1: Boolean = true
    * }}}
    */
  def fails[A: Testable](n: Int)(p: A): Boolean = !holds(n)(p)

  /** There exists an assignment of values that satisfies a property up to a number of test values?
    *
    * {{{
    * scala> exists(10) { (n: Int) => n % 3 == 0 }
    * res0: Boolean = true
    *
    * scala> exists(10) { (n: Int) => n != n }
    * res1: Boolean = false
    * }}}
    */
  def exists[A: Testable](n: Int)(p: A): Boolean =
    Testable.results[A](p).take(n).exists(_.isOk)
}
