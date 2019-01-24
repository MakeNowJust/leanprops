package leanprops

import utest._

object ListableTest extends TestSuite {
  import Function._

  val tests = Tests {
    'unit - assert(Listable.list[Unit].toList == List(()))

    'boolean - assert(Listable.list[Boolean].toList == List(true, false))

    'integral - {
      'byte - assert(
        Listable.list[Byte].take(5).toList == List(0, 1, -1, 2, -2))
      'short - assert(
        Listable.list[Short].take(5).toList == List(0, 1, -1, 2, -2))
      'int - assert(Listable.list[Int].take(5).toList == List(0, 1, -1, 2, -2))
      'long - assert(
        Listable.list[Long].take(5).toList == List(0, 1, -1, 2, -2))
      'bigInt - assert(
        Listable.list[BigInt].take(5).toList == List(0, 1, -1, 2, -2).map(
          BigInt(_)))
    }

    'fractional {
      'float - assert(
        Listable.list[Float].take(5).toList == List(0.0,
                                                    1.0,
                                                    -1.0,
                                                    Float.PositiveInfinity,
                                                    0.5))
      'double - assert(
        Listable.list[Double].take(5).toList == List(0.0,
                                                     1.0,
                                                     -1.0,
                                                     Double.PositiveInfinity,
                                                     0.5))
      'bigDecimal - assert(
        Listable.list[BigDecimal].take(5).toList == List(0, 1, -1, 0.5, 2).map(
          BigDecimal(_)))
    }

    'option - assert(
      Listable.list[Option[Int]].take(5).toList == List(None,
                                                        Some(0),
                                                        Some(1),
                                                        Some(-1),
                                                        Some(2)))

    'either - {
      val actual = Listable.list[Either[Boolean, Int]].take(5).toList
      val expected =
        List(Left(true), Right(0), Left(false), Right(1), Right(-1))
      assert(actual == expected)
    }

    'tuple - {
      'tuple1 - {
        val actual   = Listable.list[Tuple1[Boolean]].take(5).toList
        val expected = List(Tuple1(true), Tuple1(false))
        assert(actual == expected)
      }

      'tuple2 - {
        val actual = Listable.list[(Boolean, Int)].take(5).toList
        val expected =
          List((true, 0), (false, 0), (true, 1), (false, 1), (true, -1))
        assert(actual == expected)
      }

      'tuple3 - {
        val actual = Listable.list[(Boolean, Int, Boolean)].take(5).toList
        val expected = List((true, 0, true),
                            (true, 0, false),
                            (false, 0, true),
                            (false, 0, false),
                            (true, 1, true))
        assert(actual == expected)
      }

      'tuple4 - {
        val actual = Listable.list[(Boolean, Int, Boolean, Int)].take(5).toList
        val expected =
          List((true, 0, true, 0),
               (true, 0, false, 0),
               (false, 0, true, 0),
               (false, 0, false, 0),
               (true, 0, true, 1))
        assert(actual == expected)
      }

      'tuple5 - {
        val actual =
          Listable.list[(Boolean, Int, Boolean, Int, Boolean)].take(5).toList
        val expected =
          List((true, 0, true, 0, true),
               (true, 0, true, 0, false),
               (true, 0, false, 0, true),
               (true, 0, false, 0, false),
               (false, 0, true, 0, true))
        assert(actual == expected)
      }
    }

    'collection - {
      'list - {
        val actual = Listable.list[List[Boolean]].take(5).toList
        val expected = List(List(),
                            List(true),
                            List(false),
                            List(true, true),
                            List(true, false))
        assert(actual == expected)
      }

      'vector - {
        val actual = Listable.list[Vector[Boolean]].take(5).toList
        val expected = List(Vector(),
                            Vector(true),
                            Vector(false),
                            Vector(true, true),
                            Vector(true, false))
        assert(actual == expected)
      }

      'stream - {
        val actual = Listable.list[Stream[Boolean]].take(5).toList
        val expected = List(Stream(),
                            Stream(true),
                            Stream(false),
                            Stream(true, true),
                            Stream(true, false))
        assert(actual == expected)
      }

      'set - {
        'finite - {
          val actual   = Listable.list[Set[Boolean]].toList // All possible boolean set list is finite!
          val expected = List(Set(), Set(true), Set(false), Set(true, false))
          assert(actual == expected)
        }

        'infinite - {
          val actual   = Listable.list[Set[Int]].take(5).toList
          val expected = List(Set(), Set(0), Set(1), Set(0, 1), Set(-1))
          assert(actual == expected)
        }
      }

      'map - {
        'finite - {
          val actual = Listable.list[Map[Boolean, Boolean]].toList // All possible boolean to boolean map is finite!
          val expected = List(
            Map(),
            Map(true  -> true),
            Map(true  -> false),
            Map(false -> true),
            Map(false -> false),
            Map(true  -> true, false -> true),
            Map(true  -> true, false -> false),
            Map(true  -> false, false -> true),
            Map(true  -> false, false -> false)
          )
          assert(actual == expected)
        }

        'infinite - {
          val actual = Listable.list[Map[Int, Int]].take(5).toList
          val expected =
            List(Map(), Map(0 -> 0), Map(0 -> 1), Map(1 -> 0), Map(0 -> -1))
          assert(actual == expected)
        }
      }
    }

    'char - {
      val actual   = Listable.list[Char].take(10).toList
      val expected = List('a', '0', 'A', '1', 'b', '2', 'B', '3', 'c', '4')
      assert(actual == expected)
    }

    'string - {
      val actual = Listable.list[String].take(10).toList
      val expected =
        List("", "a", "aa", "0", "aaa", "a0", "0a", "A", "aaaa", "aa0")
      assert(actual == expected)
    }

    'function - {
      'function0 - {
        val actual   = Listable.list[() => Int].map(_.apply).take(10)
        val expected = Listable.list[Int].take(10)
        assert(actual == expected)
      }

      'function1 - {
        val actual = Listable
          .list[Boolean => Boolean]
          .map(f => Listable.list[Boolean].map(x => x -> f(x)).toMap)
          .toList
        val expected = List(
          Map(true -> true, false  -> true),
          Map(true -> false, false -> true),
          Map(true -> true, false  -> false),
          Map(true -> false, false -> false),
          Map(true -> true, false  -> false),
          Map(true -> false, false -> true)
        )
        assert(actual == expected)
      }

      'function2 - {
        val actual = Listable
          .list[(Boolean, Boolean) => Boolean]
          .take(5)
          .map(f =>
            Listable.list[(Boolean, Boolean)].map(x => x -> tupled(f)(x)).toMap)
          .toList
        val expected = List(
          Map((true, true)   -> true,
              (true, false)  -> true,
              (false, true)  -> true,
              (false, false) -> true),
          Map((true, true)   -> false,
              (true, false)  -> true,
              (false, true)  -> true,
              (false, false) -> true),
          Map((true, true)   -> true,
              (true, false)  -> false,
              (false, true)  -> true,
              (false, false) -> true),
          Map((true, true)   -> true,
              (true, false)  -> true,
              (false, true)  -> false,
              (false, false) -> true),
          Map((true, true)   -> true,
              (true, false)  -> true,
              (false, true)  -> true,
              (false, false) -> false),
        )
        assert(actual == expected)
      }

      'function3 - {
        val actual = Listable
          .list[(Boolean, Boolean, Boolean) => Boolean]
          .take(3)
          .map(
            f =>
              Listable
                .list[(Boolean, Boolean, Boolean)]
                .map(x => x -> tupled(f)(x))
                .toMap)
          .toList
        val expected = List(
          Map(
            (true, false, false)  -> true,
            (true, true, true)    -> true,
            (true, true, false)   -> true,
            (false, false, false) -> true,
            (false, true, true)   -> true,
            (false, false, true)  -> true,
            (true, false, true)   -> true,
            (false, true, false)  -> true
          ),
          Map(
            (true, false, false)  -> true,
            (true, true, true)    -> false,
            (true, true, false)   -> true,
            (false, false, false) -> true,
            (false, true, true)   -> true,
            (false, false, true)  -> true,
            (true, false, true)   -> true,
            (false, true, false)  -> true
          ),
          Map(
            (true, false, false)  -> true,
            (true, true, true)    -> true,
            (true, true, false)   -> false,
            (false, false, false) -> true,
            (false, true, true)   -> true,
            (false, false, true)  -> true,
            (true, false, true)   -> true,
            (false, true, false)  -> true
          )
        )
        assert(actual == expected)
      }

      'function3 - {
        val f = Listable
          .list[(Boolean, Boolean, Boolean) => Boolean]
          .head
        val actual = Listable
          .list[(Boolean, Boolean, Boolean)]
          .map(x => x -> tupled(f)(x))
          .toMap
        val expected = Map(
          (true, false, false)  -> true,
          (true, true, true)    -> true,
          (true, true, false)   -> true,
          (false, false, false) -> true,
          (false, true, true)   -> true,
          (false, false, true)  -> true,
          (true, false, true)   -> true,
          (false, true, false)  -> true
        )
        assert(actual == expected)
      }

      'function4 - {
        val f = Listable
          .list[(Boolean, Boolean, Boolean, Boolean) => Boolean]
          .head
        val actual = Listable
          .list[(Boolean, Boolean, Boolean, Boolean)]
          .map(x => x -> tupled(f)(x))
          .toMap
        val expected = Map(
          (true, false, false, false)  -> true,
          (false, false, true, true)   -> true,
          (false, true, false, true)   -> true,
          (false, true, true, true)    -> true,
          (true, true, true, false)    -> true,
          (true, false, true, false)   -> true,
          (false, true, false, false)  -> true,
          (true, true, false, false)   -> true,
          (false, false, false, true)  -> true,
          (false, false, false, false) -> true,
          (true, true, true, true)     -> true,
          (true, true, false, true)    -> true,
          (true, false, false, true)   -> true,
          (true, false, true, true)    -> true,
          (false, true, true, false)   -> true,
          (false, false, true, false)  -> true
        )
        assert(actual == expected)
      }

      'function5 - {
        val f = Listable
          .list[(Boolean, Boolean, Boolean, Boolean, Boolean) => Boolean]
          .head
        val actual = Listable
          .list[(Boolean, Boolean, Boolean, Boolean, Boolean)]
          .map(x => x -> tupled(f)(x))
          .toMap
        val expected = Map(
          (true, true, true, true, true)      -> true,
          (true, false, true, false, false)   -> true,
          (false, true, true, true, false)    -> true,
          (true, true, false, false, false)   -> true,
          (true, false, false, false, true)   -> true,
          (false, true, true, false, false)   -> true,
          (false, true, false, true, true)    -> true,
          (true, false, false, false, false)  -> true,
          (true, true, true, false, true)     -> true,
          (false, false, false, false, false) -> true,
          (false, false, true, true, true)    -> true,
          (false, true, true, false, true)    -> true,
          (false, false, false, false, true)  -> true,
          (true, true, false, true, true)     -> true,
          (true, true, false, false, true)    -> true,
          (false, false, true, false, true)   -> true,
          (false, true, false, false, true)   -> true,
          (false, false, true, false, false)  -> true,
          (true, false, true, true, false)    -> true,
          (false, true, false, false, false)  -> true,
          (true, true, true, true, false)     -> true,
          (false, true, false, true, false)   -> true,
          (false, false, false, true, false)  -> true,
          (true, false, false, true, true)    -> true,
          (false, true, true, true, true)     -> true,
          (true, true, true, false, false)    -> true,
          (false, false, true, true, false)   -> true,
          (true, false, false, true, false)   -> true,
          (true, true, false, true, false)    -> true,
          (true, false, true, false, true)    -> true,
          (false, false, false, true, true)   -> true,
          (true, false, true, true, true)     -> true
        )
        assert(actual == expected)
      }
    }
  }
}
