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
      .sequenceResults(Testable.results[A](p).take(n).filterNot(_.isOk))
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
      .sequenceResults(Testable.results[A](p).take(n).filter(_.isOk))
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

  /** Formats list of strings which `counterExample` and `witness` returns.
    *
    * {{{
    * scala> formatValues(Seq(Seq("true")))
    * res0: String = true
    *
    * scala> formatValues(Seq(Seq("true", "false"), Seq("1")))
    * res1: String = (true, false)(1)
    *
    * scala> formatValues(Seq.empty)
    * res2: String = ()
    * }}}
    */
  def formatValues(vs: Seq[Seq[String]]): String = vs match {
    case Seq(Seq(v)) => v
    case _           => vs.map(_.mkString(", ")).mkString("(", ")(", ")")
  }

  /** Checks the given property and throws a `ChecksError` with the first counter example if counter examples are found.
    *
    * {{{
    * scala> try { check(20) { (xs: List[Int]) => xs.sorted == xs }; "succeeded" }
    *      | catch { case e: CheckError => e.message }
    * res0: String = the given property failed on following argument(s): List(1, 0)
    * }}}
    *
    * @throws CheckError when counter examples are found
    */
  def check[A: Testable](n: Int)(p: A): Unit = counterExample(n)(p) match {
    case None => ()
    case Some(vs) => {
      val args = formatValues(vs)
      val message =
        s"the given property failed on following argument(s): $args"
      throw CheckError(message, vs)
    }
  }

  case class CheckError(message: String, counterExample: Seq[Seq[String]]) extends AssertionError(message)

  /** Checks the given property and throws a `ChecksError` if counter examples are found.
    *
    * {{{
    * scala> try { checks(20) { (xs: List[Int]) => xs.sorted == xs }; "succeeded" }
    *      | catch { case e: ChecksError => e.message }
    * res0: String =
    * the given property failed on following argument(s):
    *   - List(1, 0)
    *   - List(0, 1, 0)
    *   - List(0, -1)
    *   - List(1, 0, 0)
    *   - List(0, 0, 1, 0)
    *   - List(0, 0, -1)
    * }}}
    *
    * @throws ChecksError when counter examples are found
    */
  def checks[A: Testable](n: Int)(p: A): Unit = counterExamples(n)(p) match {
    case Nil => ()
    case vss => {
      val lines = vss.map(vs => s"  - ${formatValues(vs)}").mkString("\n")
      val message =
        s"the given property failed on following argument(s):\n$lines"
      throw ChecksError(message, vss)
    }
  }

  case class ChecksError(message: String, counterExamples: Seq[Seq[Seq[String]]]) extends AssertionError(message)
}
