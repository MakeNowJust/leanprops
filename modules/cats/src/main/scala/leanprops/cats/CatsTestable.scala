package leanprops
package cats

import _root_.cats._
import _root_.cats.implicits._
import scala.util.{Success, Failure}

trait CatsTestable {
  import Testable._

  implicit val catsTestableResultInstances: Functor[Result] = new Functor[Result] {
    def map[A, B](r: Result[A])(f: A => B): Result[B] =
      Result(r.values.map(_.map(f)), r.ok)
  }

  implicit def catsTestableResultEq[A: Eq]: Eq[Result[A]] = new Eq[Result[A]] {
    def eqv(r1: Result[A], r2: Result[A]): Boolean =
      Eq.eqv(r1.values.toList.map(_.toList), r2.values.toList.map(_.toList)) &&
        ((r1.ok, r2.ok) match {
          case (Success(o1), Success(o2)) => Eq.eqv(o1, o2)
          case (Failure(e1), Failure(e2)) => e1 == e2
          case _                          => false
        })
  }

  implicit def catsTestableResultShow[A: Show]: Show[Result[A]] = new Show[Result[A]] {
    def show(r: Result[A]): String = {
      val vs = r.values.map(_.map(Show[A].show).mkString("Seq(", ", ", ")")).mkString("Seq(", ", ", ")")
      val ok = r.ok.map(Show[Boolean].show).toString
      s"Binding($vs, $ok)"
    }
  }
}
