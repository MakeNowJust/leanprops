package leanprops
package cats

import org.scalacheck._

import scala.util.Try

object arbitrary {
  import Inspectable._
  import Testable._

  implicit def catsTiersArbitrary[A: Arbitrary]: Arbitrary[Tiers[A]] =
    Arbitrary(Arbitrary.arbitrary[Stream[List[A]]].map(Tiers.from(_)))
  implicit def catsTiersCogen[A: Cogen]: Cogen[Tiers[A]] =
    Cogen[Stream[List[A]]].contramap(_.asStream.map(_.toList))

  implicit def catsInspectableBindingArbitrary[A: Arbitrary]: Arbitrary[Binding[A]] =
    Arbitrary(
      for {
        vs <- Arbitrary.arbitrary[List[List[A]]]
        r  <- Arbitrary.arbitrary[A]
      } yield Binding(vs, Try(r))
    )
  implicit def catsInspectableBindingCogen[A: Cogen]: Cogen[Binding[A]] =
    Cogen[(List[List[A]], Option[A])].contramap { case Binding(vs, r) => (vs.map(_.toList).toList, r.toOption) }

  implicit def catsTestableResultArbitrary[A: Arbitrary]: Arbitrary[Result[A]] =
    Arbitrary(
      for {
        vs <- Arbitrary.arbitrary[List[List[A]]]
        ok <- Arbitrary.arbitrary[Boolean]
      } yield Result(vs, Try(ok))
    )
  implicit def catsTestableResultCogen[A: Cogen]: Cogen[Result[A]] =
    Cogen[(List[List[A]], Option[Boolean])].contramap { case Result(vs, ok) => (vs.map(_.toList).toList, ok.toOption) }
}
