package leanprops
package magnolia

import utest._

import scala.util.{Success, Failure}

//
// Suit:

sealed abstract class Suit

case object ♠ extends Suit
case object ♣ extends Suit
case object ♢ extends Suit
case object ♡ extends Suit

//
// Card:

case class Card(suit: Suit, number: Card.Number)

object Card {
  val A = Number(1)
  val J = Number(11)
  val Q = Number(12)
  val K = Number(13)

  case class Number(value: Int)

  object Number {
    implicit val listable: Listable[Number] =
      Listable.fromList((1 to 13).map(Number(_)))

    implicit val inspectable: Inspectable[Number] =
      Inspectable.fromInspect {
        case Number(1)  => WithInspectConfig.pure("A")
        case Number(11) => WithInspectConfig.pure("J")
        case Number(12) => WithInspectConfig.pure("Q")
        case Number(13) => WithInspectConfig.pure("K")
        case Number(n)  => Inspectable[Int].inspect(n)
      }
  }
}

//
// Tree

sealed abstract class Tree[+T]

case object Tip                                      extends Tree[Nothing]
case class Leaf[+T](value: T)                        extends Tree[T]
case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

//
// Nonsense:

sealed trait X[A]
case class X1[A](x: X[A])      extends X[A]
case class X2[A](a1: A, a2: A) extends X[A]

sealed trait Y[A]
case class Y1[A](z: Z[A])                             extends Y[A]
case class Y2[A](a1: A, a2: () => A, a3: (A, A) => A) extends Y[A]

case class Z[A](y: Y[A])

object MagnoliaTest extends TestSuite {
  val tests = Tests {
    'suit - {
      'listable - {
        val actual   = Listable.list[Suit].toList
        val expected = List(♠, ♣, ♢, ♡)
        assert(actual == expected)
      }

      'inspectable - {
        '♠ - assert(inspect(♠) == "♠")
        '♣ - assert(inspect(♣) == "♣")
        '♢ - assert(inspect(♢) == "♢")
        '♡ - assert(inspect(♡) == "♡")
      }
    }

    'card - {
      'listable - {
        val actual = Listable.list[Card].take(10).toList
        val expected = List(
          Card(♠, Card.Number(1)),
          Card(♣, Card.Number(1)),
          Card(♢, Card.Number(1)),
          Card(♡, Card.Number(1)),
          Card(♠, Card.Number(2)),
          Card(♣, Card.Number(2)),
          Card(♢, Card.Number(2)),
          Card(♡, Card.Number(2)),
          Card(♠, Card.Number(3)),
          Card(♣, Card.Number(3))
        )
        assert(actual == expected)
      }

      'inspectable - {
        'J - assert(inspect(Card(♡, Card.A)) == "Card(♡, A)")
        'J - assert(inspect(Card(♢, Card.J)) == "Card(♢, J)")
        'Q - assert(inspect(Card(♣, Card.Q)) == "Card(♣, Q)")
        'K - assert(inspect(Card(♠, Card.K)) == "Card(♠, K)")
        'n - assert(inspect(Card(♠, Card.Number(3))) == "Card(♠, 3)")
      }
    }

    'tree - {
      'listable - {
        val actual = Listable.list[Tree[Int]].take(10).toList
        val expected = List(Tip,
                            Leaf(0),
                            Leaf(1),
                            Leaf(-1),
                            Branch(Tip, Tip),
                            Branch(Tip, Leaf(0)),
                            Branch(Leaf(0), Tip),
                            Branch(Leaf(0), Leaf(0)),
                            Leaf(2),
                            Branch(Tip, Leaf(1)))
        assert(actual == expected)
      }

      'inspectable - {
        'tip - assert(inspect(Tip) == "Tip")
        'leaf - assert(inspect(Leaf(1)) == "Leaf(1)")
        'branch - assert(inspect(Branch(Tip, Leaf(1))) == "Branch(Tip, Leaf(1))")
      }
    }

    'success - assert(inspect(Success(1)) == "Success(1)")

    'nonsense - {
      'x - {
        val actual = Listable[X[Int]].tiers.asStream.take(3).toList
        val expected = List(List(X2(0, 0)),
                            List(X1(X2(0, 0)), X2(0, 1), X2(1, 0)),
                            List(X1(X1(X2(0, 0))), X1(X2(0, 1)), X1(X2(1, 0)), X2(0, -1), X2(1, 1), X2(-1, 0)))
        assert(actual == expected)
      }

      'yz - {
        def toA[A](y: Y[A]): A = y match {
          case Y1(Z(y))       => toA(y)
          case Y2(a1, a2, a3) => a3(a1, a2())
        }
        val actual   = Listable.list[Y[Int]].take(10).toList.map(toA)
        val expected = List(0, 0, 0, 0, 0, 0, 0, 1, 1, 0)
        assert(actual == expected)
      }
    }
  }
}
