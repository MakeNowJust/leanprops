package leanprops
package cats

import _root_.cats._
import _root_.cats.implicits._

trait CatsTiers {
  // Unfortunately current `Tiers#flatMapT` implementation doesn't satisfy Monad laws...

  implicit val catsTiersInstances: Traverse[Tiers] with CoflatMap[Tiers] with MonoidK[Tiers] =
    new Traverse[Tiers] with CoflatMap[Tiers] with MonoidK[Tiers] {
      def empty[A]: Tiers[A]                              = Tiers.empty[A]
      def combineK[A](t: Tiers[A], u: Tiers[A]): Tiers[A] = t \++/ u
      def coflatMap[A, B](t: Tiers[A])(f: Tiers[A] => B): Tiers[B] = {
        def loop(t: Tiers[A]): Tiers[B] =
          if (t.isEmpty) Tiers.empty
          else if (t.head.isEmpty) Tiers.delay(loop(t.tail))
          else Tiers.cons0(f(t)) \++/ loop(Tiers.from(t.head.tail #:: t.tail.asStream))
        loop(t)
      }
      def traverse[G[_], A, B](t: Tiers[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tiers[B]] = {
        def loop(tg: Tiers[G[B]]): G[Tiers[B]] =
          if (tg.isEmpty) G.pure(Tiers.empty[B])
          else if (tg.head.isEmpty) loop(tg.tail).map(Tiers.delay(_))
          else (tg.head.head.map(Tiers.cons0), loop(Tiers.from(tg.head.tail #:: tg.tail.asStream))).mapN(_ \++/ _)
        loop(t.map(f))
      }
      def foldLeft[A, B](ta: Tiers[A], b: B)(f: (B, A) => B): B =
        Traverse[Stream].foldLeft(ta.list, b)(f)
      def foldRight[A, B](ta: Tiers[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Traverse[Stream].foldRight(ta.list, b)(f)
    }

  implicit def catsTiersEq[A: Eq]: Eq[Tiers[A]] = Eq.by(_.asStream.map(_.toList))

  implicit def catsTiersMonoid[A]: Monoid[Tiers[A]] = new Monoid[Tiers[A]] {
    def empty: Tiers[A]                             = Tiers.empty
    def combine(t: Tiers[A], u: Tiers[A]): Tiers[A] = t \++/ u
  }

  implicit def catsTiersShow[A](implicit A: Show[A]): Show[Tiers[A]] = Show.show { t =>
    // Show only the first 5 tiers and "...".
    val more = if (t.asStream.drop(5).nonEmpty) ", ..." else ""
    t.asStream
      .take(5)
      .map(_.toList.map(A.show).mkString("Seq(", ", ", ")"))
      .mkString("Tiers(", ", ", more ++ ")")
  }
}

object CatsTiers extends CatsTiers
