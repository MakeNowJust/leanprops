package leanprops

import scala.util.{Try, Success, Failure}

/** `Testable` values are functions of 'Listable' arguments that return boolean value. */
trait Testable[P] {
  import Testable._
  import Inspectable._

  def resultiers(x: Try[P]): Tiers[Result[WithInspectConfig[String]]]
}

object Testable {

  import Listable._
  import Inspectable._
  import Tiers._

  case class Result[A](values: Seq[Seq[A]], ok: Try[Boolean]) {
    def isOk: Boolean = ok.getOrElse(false)
  }

  /** Creates `Testable` instance from `resultiers` function. */
  def from[P](f: Try[P] => Tiers[Result[WithInspectConfig[String]]]): Testable[P] =
    new Testable[P] {
      def resultiers(x: Try[P]): Tiers[Result[WithInspectConfig[String]]] = f(x)
    }

  /** Returns context-bounded `Testable` instance. */
  def apply[P: Testable]: Testable[P] = implicitly[Testable[P]]

  /** Returns (infinite) results of the given property. */
  def results[P: Testable](p: P): Seq[Result[WithInspectConfig[String]]] =
    Testable[P].resultiers(Success(p)).list

  implicit val BooleanTestable: Testable[Boolean] = from(x => cons0(Result(Seq.empty, x)))

  implicit def Function0Testable[P: Testable]: Testable[() => P] =
    from(x =>
      Testable[P].resultiers(x.map(p => p())).map {
        case Result(vs, r) => Result(Seq.empty +: vs, r)
    })

  implicit def Function1Testable[A: Listable: Inspectable, P: Testable]: Testable[A => P] =
    from(x =>
      tiers[A].flatMapT(a =>
        Testable[P].resultiers(x.map(p => p(a))).map {
          case Result(vs, r) => Result(Seq(inspect(a)) +: vs, r)
      }))

  implicit def Function2Testable[A: Listable: Inspectable, B: Listable: Inspectable, P: Testable]
    : Testable[(A, B) => P] =
    from { x =>
      tiers[(A, B)].flatMapT {
        case (a, b) =>
          Testable[P].resultiers(x.map(p => p(a, b))).map {
            case Result(vs, r) => Result(Seq(inspect(a), inspect(b)) +: vs, r)
          }
      }
    }

  implicit def Function3Testable[A: Listable: Inspectable,
                                 B: Listable: Inspectable,
                                 C: Listable: Inspectable,
                                 P: Testable]: Testable[(A, B, C) => P] =
    from { x =>
      tiers[(A, B, C)].flatMapT {
        case (a, b, c) =>
          Testable[P].resultiers(x.map(p => p(a, b, c))).map {
            case Result(vs, r) =>
              Result(Seq(inspect(a), inspect(b), inspect(c)) +: vs, r)
          }
      }
    }

  implicit def Function4Testable[A: Listable: Inspectable,
                                 B: Listable: Inspectable,
                                 C: Listable: Inspectable,
                                 D: Listable: Inspectable,
                                 P: Testable]: Testable[(A, B, C, D) => P] =
    from { x =>
      tiers[(A, B, C, D)].flatMapT {
        case (a, b, c, d) =>
          Testable[P].resultiers(x.map(p => p(a, b, c, d))).map {
            case Result(ss, r) =>
              Result(Seq(inspect(a), inspect(b), inspect(c), inspect(d)) +: ss, r)
          }
      }
    }

  implicit def Function5Testable[A: Listable: Inspectable,
                                 B: Listable: Inspectable,
                                 C: Listable: Inspectable,
                                 D: Listable: Inspectable,
                                 E: Listable: Inspectable,
                                 P: Testable]: Testable[(A, B, C, D, E) => P] =
    from { x =>
      tiers[(A, B, C, D, E)].flatMapT {
        case (a, b, c, d, e) =>
          Testable[P].resultiers(x.map(p => p(a, b, c, d, e))).map {
            case Result(ss, r) =>
              Result(Seq(inspect(a), inspect(b), inspect(c), inspect(d), inspect(e)) +: ss, r)
          }
      }
    }
}
