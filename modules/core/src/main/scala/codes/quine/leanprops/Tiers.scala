package codes.quine.leanprops

/** A wrapper class of `Stream[Seq[A]]`. It provides some utility operators.
  *
  * You can construct `Tiers` from values by these methods:
  *   - `Tiers.empty`
  *   - `Tiers.apply` (a.k.a. `Tiers(...)`),
  *   - `Tiers.from`,
  *   - `Tiers.of` and `Tiers.cons0`
  */
final case class Tiers[A] private[Tiers] (asStream: Stream[Seq[A]])
    extends AnyVal { self =>
  import Tiers._

  /** Returns the flatten tiers. */
  def list: Stream[A] = self.asStream.flatten

  //
  // Utility methods:

  /** Is this `Tiers` empty?
    *
    * {{{
    * scala> Tiers.empty.isEmpty
    * res0: Boolean = true
    *
    * scala> Tiers.cons0(1).isEmpty
    * res1: Boolean = false
    * }}}
    */
  def isEmpty: Boolean = self.asStream.isEmpty

  /** Gets the first tier. */
  def head: Seq[A] = self.asStream.head

  /** Drops the first tier. */
  def tail: Tiers[A] = from(self.asStream.tail)

  /** Drops thee last tier. */
  def init: Tiers[A] = from(self.asStream.init)

  //
  // General combinator:

  /** `map` each tier. */
  def mapTier[B](f: Seq[A] => Seq[B]): Tiers[B] = from(self.asStream.map(f))

  /** `filter` each tier. */
  def filterTier(f: Seq[A] => Boolean): Tiers[A] = from(self.asStream.filter(f))

  /** `map` over tiers.
    *
    * {{{
    * scala> Tiers.fromList(1 to 3).map(_ + 1)
    * res0: Tiers[Int] = Tiers(List(2), List(3), List(4))
    * }}}
    */
  def map[B](f: A => B): Tiers[B] = from(self.asStream.map(_.map(f)))

  /** `filter` over tiers.
    *
    * {{{
    * scala> Tiers.fromList(1 to 3).filter(_ % 2 == 0)
    * res0: Tiers[Int] = Tiers(List(), List(2), List())
    * }}}
    */
  def filter(f: A => Boolean): Tiers[A] = from(self.asStream.map(_.filter(f)))

  /** `flatMap` over tiers.
    *
    * {{{
    * scala> import Tiers._
    * import Tiers._
    *
    * scala> Tiers.fromList(1 to 3).flatMap(cons0)
    * res0: Tiers[Int] = Tiers(List(1), List(2), List(3))
    *
    * scala> holds(100) { (t: Tiers[Tiers[Int]]) =>
    *      |   t.flatMap(identity) == flatten(t)
    *      | }
    * res1: Boolean = true
    * }}}
    */
  def flatMap[B](f: A => Tiers[B]): Tiers[B] = flatten(self.map(f))

  //
  // Merge combinator:

  /** Merges two tiers by the given function.
    *
    * {{{
    * scala> import Tiers._
    * import Tiers._
    *
    * scala> Tiers.of(1 to 10: _*).mergeWith(Tiers.of(5 to 15: _*))(_ intersect _)
    * res0: Tiers[Int] = Tiers(List(5, 6, 7, 8, 9, 10))
    * }}}
    *
    * `\++/` and `\+|/` can be defined by this method.
    *
    * {{{
    * scala> holds(100) { (t: Tiers[Int], u: Tiers[Int]) =>
    *      |   (t \++/ u) == t.mergeWith(u)(_ ++ _)
    *      | }
    * res0: Boolean = true
    *
    * scala> holds(100) { (t: Tiers[Int], u: Tiers[Int]) =>
    *      |   (t \+|/ u) == t.mergeWith(u)(_ +| _)
    *      | }
    * res1: Boolean = true
    * }}}
    */
  def mergeWith(that: Tiers[A])(f: (Seq[A], Seq[A]) => Seq[A]): Tiers[A] = {
    def loop(self: Stream[Seq[A]], that: Stream[Seq[A]]): Stream[Seq[A]] =
      if (self.isEmpty) that
      else if (that.isEmpty) self
      else f(self.head, that.head) #:: loop(self.tail, that.tail)

    Tiers(loop(self.asStream, that.asStream))
  }

  /** Merges two tiers by `++`.
    *
    * {{{
    * scala> Tiers.fromList(1 to 3) \++/ Tiers.fromList(4 to 6)
    * res0: Tiers[Int] = Tiers(List(1, 4), List(2, 5), List(3, 6))
    * }}}
    */
  def \++/(that: Tiers[A]): Tiers[A] = self.mergeWith(that)(_ ++ _)

  /** Merges two tiers by `+|`.
    *
    * {{{
    * scala> Tiers.of(1 to 3: _*) \+|/ Tiers.of(4 to 6: _*)
    * res0: Tiers[Int] = Tiers(List(1, 4, 2, 5, 3, 6))
    * }}}
    */
  def \+|/(that: Tiers[A]): Tiers[A] = self.mergeWith(that)(_ +| _)

  //
  // Product operator:

  /** Take a tiered product of lists of tiers.
    *
    * {{{
    * scala> Tiers.fromList(1 to 3).productWith(Tiers.fromList(11 to 13))((_, _))
    * res0: Tiers[(Int, Int)] = Tiers(List((1,11)), List((1,12), (2,11)), List((1,13), (2,12), (3,11)), List((2,13), (3,12)), List((3,13)))
    *
    * scala> holds(100) { (t: Tiers[Int], u: Tiers[Int]) =>
    *      |   t.productWith(u)((_, _)) == (t >< u)
    *      | }
    * res0: Boolean = true
    * }}}
    */
  def productWith[B, C](that: Tiers[B])(f: (A, B) => C): Tiers[C] =
    if (self.isEmpty || that.isEmpty) Tiers.empty
    else {
      def product(xs: Seq[A], ys: Seq[B]): Seq[C] =
        for {
          x <- xs
          y <- ys
        } yield f(x, y)

      that.mapTier(product(self.head, _)) \++/ delay(
        self.tail.productWith(that)(f))
    }

  /** Take a tiered product of lists of tiers. */
  def ><[B](that: Tiers[B]): Tiers[(A, B)] = self.productWith(that)((_, _))

  //
  // Choice combinator:

  /** Lists tiers of choices.
    * Choices are pairs of values and tiers excluding that value.
    *
    * {{{
    * scala> Tiers.of(true, false).choice
    * res0: Tiers[(Boolean, Tiers[Boolean])] = Tiers(List((true,Tiers(List(false)))), List((false,Tiers(List(true)))))
    * }}}
    */
  def choice: Tiers[(A, Tiers[A])] = self.choiceWith((_, _))

  /** Like `choice`, but allows a custom function. */
  def choiceWith[B](f: (A, Tiers[A]) => B): Tiers[B] = self.asStream match {
    case Seq()      => Tiers.empty
    case Seq(Seq()) => Tiers.empty
    case Seq() +: xss =>
      delay(Tiers(xss).choiceWith((y, yss) => f(y, delay(yss.normalize))))
    case (x +: xs) +: xss =>
      cons0(f(x, Tiers(xs +: xss))) \++/
        delay(Tiers(xs +: xss).choiceWith {
          case (y, Tiers(ys +: yss)) => f(y, Tiers((x +: ys) +: yss))
        })
  }

  /** Like `choice` but lists tiers of strictly ascending choices. */
  def setChoice: Tiers[(A, Tiers[A])] = self.setChoiceWith((_, _))

  /** Like `setChoice` but customized by a function. */
  def setChoiceWith[B](f: (A, Tiers[A]) => B): Tiers[B] = self.asStream match {
    case Seq()      => Tiers.empty
    case Seq(Seq()) => Tiers.empty
    case Seq() +: xss =>
      delay(Tiers(xss).setChoiceWith((y, yss) => f(y, delay(yss.normalize))))
    case (x +: xs) +: xss =>
      cons0(f(x, Tiers(xs #:: xss))) \++/ Tiers(xs #:: xss).setChoiceWith(f)
  }

  /** Normalizes tiers by removing up to 5 empty tiers from the end of a list of tiers. */
  def normalize: Tiers[A] = {
    def loop(self: Stream[Seq[A]]): Stream[Seq[A]] = self match {
      case Seq()                                  => Stream.empty
      case Seq(Seq())                             => Stream.empty
      case Seq(Seq(), Seq())                      => Stream.empty
      case Seq(Seq(), Seq(), Seq())               => Stream.empty
      case Seq(Seq(), Seq(), Seq(), Seq())        => Stream.empty
      case Seq(Seq(), Seq(), Seq(), Seq(), Seq()) => Stream.empty
      case xs +: xss                              => xs #:: loop(xss)
    }

    Tiers(loop(self.asStream))
  }

  //
  // Weight combinator:

  /** Reset weight as `n`. */
  def ofWeight(n: Int): Tiers[A] = addWeight(n)(reset(self))

  //
  // toString override:

  override def toString: String = self.toString(5)

  def toString(n: Int): String = {
    // Show only the first N tiers and "...".
    val more = if (self.asStream.drop(n).nonEmpty) ", ..." else ""
    self.asStream
      .take(n)
      .map(_.toList.toString)
      .mkString("Tiers(", ", ", more ++ ")")
  }
}

object Tiers {

  import Function._

  //
  // Constructor:

  /** Returns an empty `Tiers`.
    *
    * {{{
    * scala> Tiers.empty[Int]
    * res0: Tiers[Int] = Tiers()
    * }}}
    */
  def empty[A]: Tiers[A] = from(Stream.empty)

  def from[A](s: Stream[Seq[A]]): Tiers[A] = Tiers(s)

  def apply[A](s: Seq[A]*): Tiers[A] = from(s.toStream)

  /** Returns a `Tiers` which have only tier containing the given values.
    *
    * Ideally, it means variadic arguments version of `cons0`.
    *
    * {{{
    * scala> Tiers.of(1 to 3: _*)
    * res0: Tiers[Int] = Tiers(List(1, 2, 3))
    * }}}
    */
  def of[A](xs: A*): Tiers[A] = from(Stream(xs))

  /** Takes a list of values and transform it into tiers on which each
    * tier is occupied by a single element.
    *
    * {{{
    * scala> Tiers.fromList(1 to 3)
    * res0: Tiers[Int] = Tiers(List(1), List(2), List(3))
    * }}}
    */
  def fromList[A](xs: Seq[A]): Tiers[A] = from(xs.toStream.map(Seq(_)))

  /** Given a constructor with no arguments,
    * returns the smallest `Tiers` containing it.
    *
    * {{{
    * scala> Tiers.cons0[Int](0)
    * res0: Tiers[Int] = Tiers(List(0))
    * }}}
    */
  def cons0[A](x: A): Tiers[A] = of(x)

  /** Given a constructor with one `Listable` argument,
    * returns `Tiers` of all possible applications of this constructor.
    * By default, returned values will have weight of 1.
    *
    * {{{
    * scala> Tiers.cons1[Boolean, Option[Boolean]](Some(_))
    * res0: Tiers[Option[Boolean]] = Tiers(List(), List(Some(true), Some(false)))
    * }}}
    */
  def cons1[A: Listable, R](f: A => R): Tiers[R] =
    delay(Listable[A].tiers.map(f))

  /** Returns tiers of applications of a 2-argument constructor. */
  def cons2[A: Listable, B: Listable, R](f: (A, B) => R): Tiers[R] =
    delay(Listable[(A, B)].tiers.map(tupled(f)))

  /** Returns tiers of applications of a 3-argument constructor. */
  def cons3[A: Listable, B: Listable, C: Listable, R](
      f: (A, B, C) => R): Tiers[R] =
    delay(Listable[(A, B, C)].tiers.map(tupled(f)))

  /** Returns tiers of applications of a 4-argument constructor. */
  def cons4[A: Listable, B: Listable, C: Listable, D: Listable, R](
      f: (A, B, C, D) => R): Tiers[R] =
    delay(Listable[(A, B, C, D)].tiers.map(tupled(f)))

  /** Returns tiers of applications of a 5-argument constructor. */
  def cons5[A: Listable, B: Listable, C: Listable, D: Listable, E: Listable, R](
      f: (A, B, C, D, E) => R): Tiers[R] =
    delay(Listable[(A, B, C, D, E)].tiers.map(tupled(f)))

  /** Takes as argument tiers of element values;
    * returns tiers of size-ordered lists of elements without repetition. */
  def setOf[A](t: Tiers[A]): Tiers[Seq[A]] =
    from(Seq(Seq.empty) #:: flatten(t.setChoiceWith((x, xss) =>
      setOf(xss).map(x +: _))).asStream)

  /** Given a constructor that takes a set of elements (as a list),
    * lists tiers of applications of this constructor. */
  def setCons[A: Listable, B](f: Seq[A] => B): Tiers[B] =
    setOf(Listable[A].tiers).map(f)

  /** Takes as arguments tiers of source and target values;
    * returns tiers of maps from the source to the target encoded as lists
    * without repetition.  */
  def mapOf[A, B](t: Tiers[A], u: Tiers[B]): Tiers[Seq[(A, B)]] =
    setOf(t).flatMap(xs => products(xs.map(const(u)): _*).map(xs.zip(_)))

  /** Given a constructor that takes a map of elements (as a list),
    * lists tiers of applications of this constructor. */
  def mapCons[A: Listable, B: Listable, C](f: Seq[(A, B)] => C): Tiers[C] =
    mapOf(Listable[A].tiers, Listable[B].tiers).map(f)

  /** Takes as arguments tiers of source and target values;
    * returns tiers of pair of partial relation map and fallback value. */
  def functionOf[A, R](t: Tiers[A], u: Tiers[R]): Tiers[(Map[A, R], R)] = {
    def exceptionPairsOf(t: Tiers[A], u: Tiers[R]): Tiers[Seq[(A, R)]] =
      setOf(t).init.flatMap(xs => products(xs.map(const(u)): _*).map(xs.zip(_)))

    u.choice.flatMap {
      case (r, yss) => exceptionPairsOf(t, yss).map(zs => (zs.toMap, r))
    }
  }

  /** Given a constructor that takes partial relation (as a map) and fallback value,
    * lists tiers of applications of this constructor. */
  def functionCons[A: Listable, R: Listable, C](
      f: (Map[A, R], R) => C): Tiers[C] =
    functionOf(Listable[A].tiers, Listable[R].tiers).map(tupled(f))

  //
  // General combinator:

  /** `flatten` the given tiers. */
  def flatten[A](tt: Tiers[Tiers[A]]): Tiers[A] = {
    def loop(st: Stream[Tiers[A]]): Tiers[A] =
      if (st.isEmpty) Tiers.empty
      else st.head \++/ delay(loop(st.tail))

    loop(tt.asStream.map(_.foldLeft(Tiers.empty[A])(_ \++/ _)))
  }

  //
  // Product combinator:

  /** Takes the product of N lists of tiers, producing lists of length N.
    *
    * Alternatively, takes as argument a list of lists of tiers of elements;
    * returns lists combining elements of each list of tiers.
    *
    * {{{
    * scala> import Tiers._
    * import Tiers._
    *
    * scala> products(fromList(1 to 2), fromList(11 to 12))
    * res1: Tiers[Seq[Int]] = Tiers(List(List(1, 11)), List(List(1, 12), List(2, 11)), List(List(2, 12)))
    * }}}
    */
  def products[A](ts: Tiers[A]*): Tiers[Seq[A]] =
    if (ts.isEmpty) cons0(Seq.empty)
    else ts.head.productWith(products(ts.tail: _*))(_ +: _)

  //
  // Weight combinator:

  /** Increase a weight. */
  def delay[A](t: => Tiers[A]): Tiers[A] = from(Seq.empty #:: t.asStream)

  /** Add weight. */
  def addWeight[A](n: Int)(t: => Tiers[A]): Tiers[A] =
    from(Stream.fill(n)(Seq.empty) #::: t.asStream)

  /** Reset weight as zero. */
  def reset[A](t: Tiers[A]): Tiers[A] = from(t.asStream.dropWhile(_.isEmpty))

  //
  // Utility functions for list:

  /** Provides utility functions for list. */
  implicit class ListOps[A](val self: Seq[A]) extends AnyVal {

    /** Interleaves two list.
      *
      * {{{
      * scala> import Tiers._
      * import Tiers._
      *
      * scala> ((1 to 3) +| (4 to 6)).toList
      * res0: List[Int] = List(1, 4, 2, 5, 3, 6)
      * }}}
      */
    def +|(that: Seq[A]): Seq[A] = (self, that) match {
      case (_, Seq())    => self
      case (Seq(), _)    => that
      case (x +: xs, ys) => x #:: (ys +| xs).toStream
    }

    /** Like `distinct` but allows customize by function.
      *
      * NOTE: It is used only for implementing `Inspectable`, so it is package private.
      */
    private[leanprops] def distinctOn(f: (A, A) => Boolean): Seq[A] = {
      def loop(self: Seq[A]): Seq[A] = self match {
        case Seq()   => Seq.empty
        case x +: xs => x +: loop(xs.filterNot(f(_, x)))
      }

      loop(self)
    }
  }
}
