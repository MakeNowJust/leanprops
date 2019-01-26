package leanprops

import scala.annotation.tailrec

/** A type is `Listable` when there exists a function that
  * is able to list (ideally all of) its values.
  *
  * Instances should be defined by 'tiers' function that
  * returns a (potentially infinite) list of finite sub-list (tiers):
  *   - the first sub-list contains elements of weight 0.
  *   - the second sub-list contains elements of weight 1.
  *   - and so on...
  * Weight is defined by the implementor of the type-class instance.
  */
trait Listable[A] {
  def tiers: Tiers[A]
}

object Listable {
  import Tiers._

  /** Creates `Listable` instance from 'tiers'. */
  def from[A](t: => Tiers[A]): Listable[A] = new Listable[A] {
    def tiers: Tiers[A] = t
  }

  /** Creates `Listable` instance from 'list'. */
  def fromList[A](s: => Seq[A]): Listable[A] = from(Tiers.fromList(s))

  /** Returns context bounded `Listable` instance. */
  def apply[A: Listable]: Listable[A] = implicitly[Listable[A]]

  /** Shortcut to `Listable[A].tiers`. */
  def tiers[A: Listable]: Tiers[A] = Listable[A].tiers

  /** Shortcut to `Listable[A].list`. */
  def list[A: Listable]: Stream[A] = tiers[A].list

  //
  // Finite enum:

  implicit val UnitListable: Listable[Unit]       = from(cons0(()))
  implicit val BooleanListable: Listable[Boolean] = from(cons0(true) \++/ cons0(false))

  //
  // Integral:

  private[this] def listIntegral[A](implicit num: Numeric[A]): Stream[A] = {
    import num._
    val succ      = (x: A) => plus(x, one)
    val pred      = (x: A) => minus(x, one)
    val positives = Stream.iterate(succ(zero))(succ).takeWhile(lt(zero, _))
    val negatives = Stream.iterate(pred(zero))(pred).takeWhile(gt(zero, _))
    zero #:: (positives +| negatives).toStream
  }

  implicit val ByteListable: Listable[Byte]     = fromList(listIntegral[Byte])
  implicit val ShortListable: Listable[Short]   = fromList(listIntegral[Short])
  implicit val IntListable: Listable[Int]       = fromList(listIntegral[Int])
  implicit val LongListable: Listable[Long]     = fromList(listIntegral[Long])
  implicit val BigIntListable: Listable[BigInt] = fromList(listIntegral[BigInt])

  //
  // Fractional:

  private[this] def tiersFractional[A](implicit frac: Fractional[A]): Tiers[A] = {
    import frac._

    @tailrec
    def gcd(x: Int, y: Int): Int = if (y == 0) Math.abs(x) else gcd(y, x % y)

    reset(
      Listable[(Int, Int)].tiers
        .filter { case (n, d) => d > 0 && gcd(n, d) == 1 }
        .map { case (x, y) => fromInt(x) / fromInt(y) })
  }

  private[this] def tiersFloating[A](implicit frac: Fractional[A]): Tiers[A] = {
    import frac._
    val inf = one / zero
    tiersFractional[A] \++/ Tiers(Stream(Seq(), Seq(), Seq(inf), Seq(-inf)))
  }

  implicit val FloatListable: Listable[Float]           = from(tiersFloating[Float])
  implicit val DoubleListable: Listable[Double]         = from(tiersFloating[Double])
  implicit val BigDecimalListable: Listable[BigDecimal] = from(tiersFractional[BigDecimal])

  //
  // Data structure:

  implicit def OptionListable[A: Listable]: Listable[Option[A]] =
    from(cons0[Option[A]](None) \++/ cons1[A, Option[A]](Some(_)))

  implicit def EitherListable[A: Listable, B: Listable]: Listable[Either[A, B]] =
    from(reset(cons1[A, Either[A, B]](Left(_))) \+|/ reset(cons1[B, Either[A, B]](Right(_))))

  //
  // Tuple:

  implicit def Tuple1Listable[A: Listable]: Listable[Tuple1[A]] =
    from(cons1[A, Tuple1[A]](Tuple1[A]))
  implicit def Tuple2Listable[A: Listable, B: Listable]: Listable[(A, B)] =
    from(tiers[A] >< tiers[B])
  implicit def Tuple3Listable[A: Listable, B: Listable, C: Listable]: Listable[(A, B, C)] =
    from(reset(cons2[(A, B), C, (A, B, C)] { case ((a, b), c) => (a, b, c) }))
  implicit def Tuple4Listable[A: Listable, B: Listable, C: Listable, D: Listable]: Listable[(A, B, C, D)] =
    from(reset(cons2[(A, B, C), D, (A, B, C, D)] {
      case ((a, b, c), d) => (a, b, c, d)
    }))
  implicit def Tuple5Listable[A: Listable, B: Listable, C: Listable, D: Listable, E: Listable]
    : Listable[(A, B, C, D, E)] =
    from(reset(cons2[(A, B, C, D), E, (A, B, C, D, E)] {
      case ((a, b, c, d), e) => (a, b, c, d, e)
    }))

  //
  // Collection:

  implicit def ListListable[A: Listable]: Listable[List[A]] =
    from(cons0[List[A]](Nil) \++/ cons2[A, List[A], List[A]](_ :: _))

  implicit def VectorListable[A: Listable]: Listable[Vector[A]] =
    from(cons0[Vector[A]](Vector.empty) \++/ cons2[A, Vector[A], Vector[A]](_ +: _))

  implicit def StreamListable[A: Listable]: Listable[Stream[A]] =
    from(cons0[Stream[A]](Stream.empty) \++/ cons2[A, Stream[A], Stream[A]](_ #:: _))

  implicit def SetListable[A: Listable]: Listable[Set[A]] =
    from(setCons[A, Set[A]](_.toSet))

  implicit def MapListable[A: Listable, B: Listable]: Listable[Map[A, B]] =
    from(mapCons[A, B, Map[A, B]](_.toMap))

  //
  // Char & String:

  implicit val CharListable: Listable[Char]     = fromList(('a' to 'z') +| ('A' to 'Z') +| ('0' to '9'))
  implicit val StringListable: Listable[String] = from(tiers[List[Char]].map(_.mkString))

  //
  // Function:

  implicit def Function0Listable[R: Listable]: Listable[() => R] =
    from(tiers[R].map(x => () => x))
  implicit def Function1Listable[A: Listable, R: Listable]: Listable[A => R] =
    from(functionCons(TableFunction1[A, R]))
  implicit def Function2Listable[A: Listable, B: Listable, R: Listable]: Listable[(A, B) => R] =
    from(functionCons(TableFunction2[A, B, R]))
  implicit def Function3Listable[A: Listable, B: Listable, C: Listable, R: Listable]: Listable[(A, B, C) => R] =
    from(functionCons(TableFunction3[A, B, C, R]))
  implicit def Function4Listable[A: Listable, B: Listable, C: Listable, D: Listable, R: Listable]
    : Listable[(A, B, C, D) => R] =
    from(functionCons(TableFunction4[A, B, C, D, R]))
  implicit def Function5Listable[A: Listable, B: Listable, C: Listable, D: Listable, E: Listable, R: Listable]
    : Listable[(A, B, C, D, E) => R] =
    from(functionCons(TableFunction5[A, B, C, D, E, R]))

  //
  // Tier:

  implicit def TiersListable[A: Listable]: Listable[Tiers[A]] =
    from(Listable[Stream[List[A]]].tiers.map(Tiers[A]))
}
