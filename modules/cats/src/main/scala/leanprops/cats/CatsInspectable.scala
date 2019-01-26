package leanprops
package cats

import _root_.cats._
import _root_.cats.implicits._
import scala.util.{Success, Failure}

trait CatsInspectable {
  import Inspectable._

  implicit val catsInspectableBindingInstances: Functor[Binding] = new Functor[Binding] {
    def map[A, B](b: Binding[A])(f: A => B): Binding[B] =
      Binding(b.values.map(_.map(f)), b.result.map(f))
  }

  implicit def catsInspectableBindingEq[A: Eq]: Eq[Binding[A]] = new Eq[Binding[A]] {
    def eqv(b1: Binding[A], b2: Binding[A]): Boolean =
      Eq.eqv(b1.values.toList.map(_.toList), b2.values.toList.map(_.toList)) &&
        ((b1.result, b2.result) match {
          case (Success(r1), Success(r2)) => Eq.eqv(r1, r2)
          case (Failure(e1), Failure(e2)) => e1 == e2
          case _                          => false
        })
  }

  implicit def catsInspectableBindingShow[A: Show]: Show[Binding[A]] = new Show[Binding[A]] {
    def show(b: Binding[A]): String = {
      val vs = b.values.map(_.map(Show[A].show).mkString("Seq(", ", ", ")")).mkString("Seq(", ", ", ")")
      val r  = b.result.map(Show[A].show).toString
      s"Binding($vs, $r)"
    }
  }
}

object CatsInspectable extends CatsInspectable
