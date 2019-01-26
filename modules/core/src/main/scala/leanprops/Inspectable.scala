package leanprops

import scala.annotation.{switch, tailrec}
import scala.util.{DynamicVariable, Failure, Success, Try}

/** A type is `Inspectable` when there exists string representation of it.
  *
  * This string representation should be ideally unique for each value on the type,
  * but some type is hard to provide such a representation (i.e. infinite `Stream`, functions and more).
  *
  * This type-class requires two methods:
  *   - `inspect`: returns a string representation simply.
  *   - `bindtiers`: returns tiers of bindings. It is used for `Inspective` of functions implementations.
  * You can use `Inspectable.fromInspect` (resp. `fromBindtiers`) which creates an `Inspectable` instance from
  * `inspect`  (resp. `bindtiers`) function and provides default `bindtiers` (resp. `inspect`) implementation.
  */
trait Inspectable[A] {
  import Inspectable._

  def inspect(v: A): WithInspectConfig[String]

  def bindtiers(x: Try[A]): Tiers[Binding[WithInspectConfig[String]]]
}

object Inspectable extends InspectableInstances with InspectableFunctionInstance {
  import WithInspectConfig._

  /** `Binding` means a pair of input value and result. */
  case class Binding[S](values: Seq[Seq[S]], result: Try[S])

  //
  // Utility function:

  /** Returns context bounded `Inspectable` config. */
  def apply[A: Inspectable]: Inspectable[A] = implicitly[Inspectable[A]]

  def inspect[A: Inspectable](v: A): WithInspectConfig[String] =
    Inspectable[A].inspect(v)

  /** Returns bindings list. */
  def bindings[A: Inspectable](x: A): Stream[Binding[WithInspectConfig[String]]] =
    Inspectable[A].bindtiers(Success(x)).list
}

private[leanprops] trait InspectableInstances {
  import Inspectable._, WithInspectConfig._

  /** Creates `Inspectable` instance from `inspect` function. */
  def fromInspect[A](f: (A) => WithInspectConfig[String]): Inspectable[A] =
    new Inspectable[A] {
      def inspect(v: A): WithInspectConfig[String] = f(v)
      def bindtiers(x: Try[A]): Tiers[Binding[WithInspectConfig[String]]] =
        Tiers.cons0(Binding(Seq.empty, x match {
          case Success(v) => Success(inspect(v))
          case Failure(e) => Failure(e)
        }))
    }

  /** Creates `Inspectable` instance using `toString` as `inspect` function. */
  def fromToString[A]: Inspectable[A] = fromInspect(v => pure(v.toString))

  implicit val UnitInspectable: Inspectable[Unit]             = fromToString
  implicit val BooleanInspectable: Inspectable[Boolean]       = fromToString
  implicit val ByteInspectable: Inspectable[Byte]             = fromToString
  implicit val ShortInspectable: Inspectable[Short]           = fromToString
  implicit val IntInspectable: Inspectable[Int]               = fromToString
  implicit val LongInspectable: Inspectable[Long]             = fromToString
  implicit val BigIntInspectable: Inspectable[BigInt]         = fromToString
  implicit val FloatInspectable: Inspectable[Float]           = fromToString
  implicit val DoubleInspectable: Inspectable[Double]         = fromToString
  implicit val BigDecimalInspectable: Inspectable[BigDecimal] = fromToString

  implicit def OptionInspectable[A: Inspectable]: Inspectable[Option[A]] =
    fromInspect[Option[A]] {
      case Some(v) => cons1("Some", v)
      case None    => cons0("None")
    }
  implicit def EitherInspectable[A: Inspectable, B: Inspectable]: Inspectable[Either[A, B]] =
    fromInspect {
      case Left(v)  => cons1("Left", v)
      case Right(v) => cons1("Right", v)
    }

  implicit def Tuple1Inspectable[A: Inspectable]: Inspectable[Tuple1[A]] =
    fromInspect(a => cons1("Tuple1", a._1))
  implicit def Tuple2Inspectable[A: Inspectable, B: Inspectable]: Inspectable[(A, B)] = fromInspect {
    case (a, b) => cons2("", a, b)
  }
  implicit def Tuple3Inspectable[A: Inspectable, B: Inspectable, C: Inspectable]: Inspectable[(A, B, C)] = fromInspect {
    case (a, b, c) => cons3("", a, b, c)
  }
  implicit def Tuple4Inspectable[A: Inspectable, B: Inspectable, C: Inspectable, D: Inspectable]
    : Inspectable[(A, B, C, D)] =
    fromInspect {
      case (a, b, c, d) => cons4("", a, b, c, d)
    }
  implicit def Tuple5Inspectable[A: Inspectable, B: Inspectable, C: Inspectable, D: Inspectable, E: Inspectable]
    : Inspectable[(A, B, C, D, E)] =
    fromInspect {
      case (a, b, c, d, e) => cons5("", a, b, c, d, e)
    }

  implicit def ListInspectable[A: Inspectable]: Inspectable[List[A]] =
    fromInspect(xs => inspects(xs).map(_.mkString("List(", ", ", ")")))
  implicit def VectorInspectable[A: Inspectable]: Inspectable[Vector[A]] =
    fromInspect(xs => inspects(xs).map(_.mkString("Vector(", ", ", ")")))
  implicit def StreamInspectable[A: Inspectable]: Inspectable[Stream[A]] =
    fromInspect {
      case xs if xs.hasDefiniteSize =>
        inspects(xs).map(_.mkString("Stream(", ", ", ")"))
      case xs =>
        for {
          c <- readConfig
          // When trying to inspect infinite Stream, it show only the first N elements.
          more = if (xs.drop(c.size).nonEmpty) ", ..." else ""
          ss <- inspects(xs.take(c.size))
        } yield ss.mkString("Stream(", ", ", more ++ ")")
    }
  implicit def SetInspectable[A: Inspectable]: Inspectable[Set[A]] =
    fromInspect(xs => inspects(xs.toSeq).map(_.mkString("Set(", ", ", ")")))
  implicit def MapInspectable[A: Inspectable, B: Inspectable]: Inspectable[Map[A, B]] =
    fromInspect { m =>
      sequence(m.map { case (x, y) => inspect2(x, y)((s, t) => s"$s -> $t") }.toSeq)
        .map(_.mkString("Map(", ", ", ")"))
    }

  private[this] def escapeChar(c: Char, singleQuote: Boolean): String =
    (c: @switch) match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\'' => if (singleQuote) "\\'" else "'"
      case '"'  => if (singleQuote) "\"" else "\\\""
      case '\\' => "\\\\"
      case _ =>
        c match {
          case _ if c.isControl => f"\\u${c.toInt}%04X"
          case _                => c.toString
        }
    }

  implicit val CharInspectable: Inspectable[Char] = fromInspect(c => pure(s"'${escapeChar(c, true)}'"))
  implicit val StringInspectable: Inspectable[String] =
    fromInspect { s =>
      pure(s.iterator.map(escapeChar(_, false)).mkString("\"", "", "\""))
    }

  implicit def Function0Inspectable[R: Inspectable]: Inspectable[() => R] =
    fromInspect { f =>
      inspect1(f())(s => s"(() => $s)")
    }

  implicit def TiersInspectable[A: Inspectable]: Inspectable[Tiers[A]] =
    fromInspect { xs =>
      for {
        c <- readConfig
        more = if (xs.asStream.drop(c.size).nonEmpty) ", ..." else ""
        ss <- sequence(
          xs.asStream
            .take(c.size)
            .map(x => inspects(x).map(_.mkString("Seq(", ", ", ")"))))
      } yield ss.mkString("Tiers(", ", ", more ++ ")")
    }
}

private[leanprops] trait InspectableFunctionInstance {
  import Function._
  import Inspectable._, WithInspectConfig._
  import Listable._

  /** Creates `Inspectable` instance from `bindtiers` function. */
  def fromBindtiers[A](f: Try[A] => Tiers[Binding[WithInspectConfig[String]]]): Inspectable[A] =
    new Inspectable[A] { self =>
      def inspect(v: A): WithInspectConfig[String]                        = inspectFunction(v)(self)
      def bindtiers(x: Try[A]): Tiers[Binding[WithInspectConfig[String]]] = f(x)
    }

  implicit def Function1Inspectable[A: Listable: Inspectable, R: Inspectable]: Inspectable[A => R] =
    fromBindtiers(x =>
      tiers[A].flatMapT(a =>
        Inspectable[R].bindtiers(x.map(f => f(a))).map {
          case Binding(vs, s) => Binding(Seq(inspect(a)) +: vs, s)
      }))
  implicit def Function2Inspectable[A: Listable: Inspectable, B: Listable: Inspectable, R: Inspectable]
    : Inspectable[(A, B) => R] =
    fromBindtiers { x =>
      tiers[(A, B)].flatMapT {
        case (a, b) =>
          Inspectable[R].bindtiers(x.map(f => f(a, b))).map {
            case Binding(vs, s) => Binding(Seq(inspect(a), inspect(b)) +: vs, s)
          }
      }
    }
  implicit def Function3Inspectable[A: Listable: Inspectable,
                                    B: Listable: Inspectable,
                                    C: Listable: Inspectable,
                                    R: Inspectable]: Inspectable[(A, B, C) => R] =
    fromBindtiers { x =>
      tiers[(A, B, C)].flatMapT {
        case (a, b, c) =>
          Inspectable[R].bindtiers(x.map(f => f(a, b, c))).map {
            case Binding(vs, s) =>
              Binding(Seq(inspect(a), inspect(b), inspect(c)) +: vs, s)
          }
      }
    }
  implicit def Function4Inspectable[A: Listable: Inspectable,
                                    B: Listable: Inspectable,
                                    C: Listable: Inspectable,
                                    D: Listable: Inspectable,
                                    R: Inspectable]: Inspectable[(A, B, C, D) => R] =
    fromBindtiers { x =>
      tiers[(A, B, C, D)].flatMapT {
        case (a, b, c, d) =>
          Inspectable[R].bindtiers(x.map(f => f(a, b, c, d))).map {
            case Binding(vs, s) =>
              Binding(Seq(inspect(a), inspect(b), inspect(c), inspect(d)) +: vs, s)
          }
      }
    }
  implicit def Function5Inspectable[A: Listable: Inspectable,
                                    B: Listable: Inspectable,
                                    C: Listable: Inspectable,
                                    D: Listable: Inspectable,
                                    E: Listable: Inspectable,
                                    R: Inspectable]: Inspectable[(A, B, C, D, E) => R] =
    fromBindtiers { x =>
      tiers[(A, B, C, D, E)].flatMapT {
        case (a, b, c, d, e) =>
          Inspectable[R].bindtiers(x.map(f => f(a, b, c, d, e))).map {
            case Binding(vs, s) =>
              Binding(Seq(inspect(a), inspect(b), inspect(c), inspect(d), inspect(e)) +: vs, s)
          }
      }
    }

  private[this] def inspectFunction[F: Inspectable](f: F): WithInspectConfig[String] =
    for {
      config <- readConfig
      bs     <- inner(sequenceBindings(bindings(f))).map(_.toStream)
    } yield {
      val checks = config.size * config.size + 1
      if (isValue(bs)) inspectValue(bs)
      else if (isConstant(checks, bs)) inspectConstant(bs)
      else inspectPartialFunction(checks, config, bs)
    }

  private[this] def inspectPartialFunction(checks: Int, config: InspectConfig, bs: Stream[Binding[String]]): String = {
    val InspectConfig(size, singleLine, _) = config
    val bs1                                = describeBindings(checks, size, bs)
    val (as, bs2)                          = clarifyBindings(bs1)
    val bs3                                = removeMatchError(bs2)
    val args                               = inspectArguments(as)
    val pattern                            = inspectValues(normalizeArguments(as))

    // Constant check against generalized bindings.
    if (bs3.isEmpty || pattern == "_") inspectConstant(bs1)
    else {
      val more = bs3.size >= size
      val body = inspectBindings(bs3.take(size), more, singleLine)

      if (singleLine) s"($args => $pattern match { ${body.mkString("; ")} })"
      else s"""($args => $pattern match {
               |  ${body.mkString("\n  ")}
               |})""".stripMargin
    }
  }

  //
  // Inspect function > binding utilities:

  private[this] def removeMatchError(bs: Seq[Binding[String]]): Seq[Binding[String]] =
    bs.filter {
      case Binding(_, Failure(e: MatchError)) => false
      case _                                  => true
    }

  private[this] def inspectBindings(bs: Seq[Binding[String]], more: Boolean, singleLine: Boolean): Seq[String] = {
    val ss   = bs.map(inspectBinding)
    val last = if (more) Seq("...") else Seq.empty
    if (singleLine) ss.map { case (p, r) => p ++ r } ++ last
    else {
      val width = ss.map(_._1.length).max
      ss.map { case (p, r) => p ++ (" " * (width - p.length)) ++ r } ++ last
    }
  }

  private[this] def inspectBinding(b: Binding[String]): (String, String) =
    (s"case ${inspectValues(b.values)}", s" => ${inspectResult(b.result)}")

  private[this] def inspectValues(vs: Seq[Seq[String]]): String = vs match {
    case Seq(x)                              => inspectAsTuple(x)
    case xs if xs.forall(_.forall(_ == "_")) => "_"
    case xs                                  => xs.map(inspectAsTuple).mkString("(", ", ", ")")
  }

  private[this] def inspectAsTuple(t: Seq[String]): String = t match {
    case Seq(x)                    => x
    case xs if xs.forall(_ == "_") => "_"
    case xs                        => xs.mkString("(", ", ", ")")
  }

  private[this] def inspectArguments(as: Seq[Seq[String]]): String =
    as.map(inspectAsTuple).mkString(" => ")

  private[this] def normalizeArguments(as: Seq[Seq[String]]): Seq[Seq[String]] =
    as.map(_.filter(_ != "_")).filter(_.nonEmpty)

  private[this] def removeUnusedValues(vs: Seq[Seq[String]], used: Seq[Seq[Boolean]]): Seq[Seq[String]] =
    zip2(vs, used).map(_.filter(_._2)).filter(_.nonEmpty).map(_.map(_._1))

  private[this] def zip2[A, B](xss: Seq[Seq[A]], yss: Seq[Seq[B]]): Seq[Seq[(A, B)]] =
    zip2With(xss, yss)((_, _))

  private[this] def zip2With[A, B, C](xss: Seq[Seq[A]], yss: Seq[Seq[B]])(f: (A, B) => C): Seq[Seq[C]] =
    xss.zip(yss).map { case (xs, ys) => xs.zip(ys).map(tupled(f)) }

  private[this] def checkUsedValues(bs: Seq[Binding[String]]): Seq[Seq[Boolean]] =
    bs.map(_.values.map(_.map(_ != "_"))).reduceLeft(zip2With(_, _) { _ || _ })

  private[this] def assignArgumentNames(used: Seq[Seq[Boolean]]): Seq[Seq[String]] = {
    def argumentNames: Stream[String] =
      Stream
        .from(0)
        .flatMap(n => Seq("x", "y", "z", "w").map(s => if (n == 0) s else s"$s$n"))

    def loop(as: Stream[String], used: Seq[Seq[Boolean]]): Seq[Seq[String]] =
      used match {
        case Seq() => Seq()
        case us +: uss =>
          as.zip(us).map { case (a, u) => if (u) a else "_" } +: loop(as.drop(us.count(identity)), uss)
      }

    loop(argumentNames, used)
  }

  private[this] def inspectResult(r: Try[String]): String = r match {
    case Success(s)                      => s
    case Failure(e: NotImplementedError) => "???"
    case Failure(e) =>
      s"throw new ${e.getClass.getSimpleName}(${inspect(e.getMessage).run(InspectConfig.EightLines)})"
  }

  //
  // Inspect function > special cases:

  private[this] def isValue(bs: Stream[Binding[String]]): Boolean = bs match {
    case Seq(Binding(Seq(), _)) => true
    case _                      => false
  }

  private[this] def inspectValue(bs: Seq[Binding[String]]): String =
    inspectResult(bs.head.result)

  private[this] def isConstant(m: Int, bs: Stream[Binding[String]]): Boolean =
    bs.take(m) match {
      case Seq()               => false
      case Binding(_, r) +: xs => xs.forall(_.result == r)
    }

  private[this] def inspectConstant(bs: Seq[Binding[String]]): String = {
    val Binding(vs, r) = bs.head
    s"(${Seq.fill(vs.size)("_").mkString(" => ")} => ${inspectResult(r)})"
  }

  //
  // Inspect function > clarify:

  private[this] def clarifyBindings(bs: Seq[Binding[String]]): (Seq[Seq[String]], Seq[Binding[String]]) = {
    val used = checkUsedValues(bs)
    (assignArgumentNames(used), bs.map {
      case Binding(vs, r) => Binding(removeUnusedValues(vs, used), r)
    })
  }

  //
  // Inspect function > describe:

  private[this] def describeBindings(m: Int, n: Int, bs: Stream[Binding[String]]): Seq[Binding[String]] = {
    val bs0 = bs.take(m)
    val bs1 = Seq(
      bs0,
      explainBindings(bs0),
      explainBindings(
        bs0
          .groupBy(b => resultSimplify(b.result))
          .values
          .toList
          .sortBy(_.size)
          .flatten),
    ).minBy(_.size)
    if (bs1.size < n) bs1 else bs0
  }

  //
  // Inspect function > explain:

  private[this] def explainBindings(bs: Seq[Binding[String]]): Seq[Binding[String]] = {
    @tailrec def explain(rs: Seq[Binding[String]], bs: Seq[Binding[String]]): Seq[Binding[String]] =
      bs match {
        case Seq() => rs.reverse
        case Binding(vs, r) +: bs1 => {
          import Tiers._
          val bs2 = generalizations(vs)
            .filter(
              gvs =>
                bs1
                  .filter(b => coveredValues(b.values, gvs))
                  .forall(b => coveredResult(b.result, r)))
            .map(gvs => Binding(gvs, r))
          val bs3 = bs2.distinctOn(covered)
          explain(bs3 ++ rs, bs1.filter(b => !bs3.exists(covered(b, _))))
        }
      }

    explain(Seq.empty, bs)
  }

  private[this] def generalizations(vs: Seq[Seq[String]]): Seq[Seq[Seq[String]]] = vs match {
    case Seq()       => Seq(Seq.empty)
    case Seq() +: ws => generalizations(ws).map(Seq.empty +: _)
    case (x +: xs) +: ws =>
      val gvs = generalizations(xs +: ws)
      gvs.flatMap {
        case gxs +: gws => Seq(("_" +: gxs) +: gws, (x +: gxs) +: gws)
      }
  }

  private[this] def covered(b: Binding[String], c: Binding[String]): Boolean =
    coveredValues(b.values, c.values) && coveredResult(b.result, c.result)

  private[this] def coveredValues(vs: Seq[Seq[String]], ws: Seq[Seq[String]]): Boolean = {
    @tailrec def check(v: Seq[String], w: Seq[String]): Boolean = (v, w) match {
      case (Seq(), Seq())       => true
      case (_ +: xs, "_" +: ys) => check(xs, ys)
      case (x +: xs, y +: ys)   => x == y && check(xs, ys)
      case _                    => false
    }

    vs.zip(ws).forall { case (v, w) => check(v, w) }
  }

  private[this] def coveredResult(x: Try[String], y: Try[String]): Boolean =
    (x, y) match {
      case (Success(s), Success(t)) => s == t
      case (Failure(e), Failure(f)) =>
        e.getClass == f.getClass && e.getMessage == f.getMessage
      case _ => false
    }

  private[this] def resultSimplify(x: Try[String]): Either[(Class[_], String), String] = x match {
    case Success(s) => Right(s)
    case Failure(e) => Left((e.getClass, e.getMessage))
  }
}
