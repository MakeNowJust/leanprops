package codes.quine.leanprops

import utest._

object InspectableTest extends TestSuite {
  def inspect4[A: Inspectable](v: A) =
    Inspectable.inspect(v).run(InspectConfig.FourCases)

  val tests = Tests {
    'unit - assert(inspect(()) == "()")

    'boolean - {
      'true - assert(inspect(true) == "true")
      'false - assert(inspect(false) == "false")
    }

    'integral - {
      'byte - assert(inspect(0: Byte) == "0")
      'short - assert(inspect(0: Short) == "0")
      'int - assert(inspect(0) == "0")
      'long - assert(inspect(0: Long) == "0")
      'bigInt - assert(inspect(BigInt(0)) == "0")
    }

    'fractional - {
      'float - assert(inspect(0.0f) == "0.0")
      'double - assert(inspect(0.0) == "0.0")
      'bigDecimal - assert(inspect(BigDecimal(0.0)) == "0.0")
    }

    'option - {
      'none - assert(inspect[Option[Int]](None) == "None")
      'some - assert(inspect[Option[Int]](Some(1)) == "Some(1)")
    }

    'either - {
      'left - assert(inspect[Either[Int, Int]](Left(1)) == "Left(1)")
      'right - assert(inspect[Either[Int, Int]](Right(1)) == "Right(1)")
    }

    'tuple - {
      'tuple1 - assert(inspect(Tuple1(true)) == "Tuple1(true)")
      'tuple2 - assert(inspect((true, 1)) == "(true, 1)")
      'tuple3 - assert(inspect((true, 1, false)) == "(true, 1, false)")
      'tuple4 - assert(inspect((true, 1, false, 2)) == "(true, 1, false, 2)")
      'tuple5 - assert(
        inspect((true, 1, false, 2, true)) == "(true, 1, false, 2, true)")
    }

    'list - {
      'empty - assert(inspect(List.empty[Int]) == "List()")
      'nonEmpty - assert(inspect(List(1, 2, 3)) == "List(1, 2, 3)")
    }

    'vector - {
      'empty - assert(inspect(Vector.empty[Int]) == "Vector()")
      'nonEmpty - assert(inspect(Vector(1, 2, 3)) == "Vector(1, 2, 3)")
    }

    'stream - {
      'empty - assert(inspect(Stream.empty[Int]) == "Stream()")
      'finite - assert(inspect(Stream(1, 2, 3)) == "Stream(1, 2, 3)")
      'infinite - {
        'four - assert(inspect4(Stream.from(1)) == "Stream(1, 2, 3, 4, ...)")
        'eight - assert(
          inspect(Stream.from(1)) == "Stream(1, 2, 3, 4, 5, 6, 7, 8, ...)")
        'nested - assert(
          inspect(Stream(Stream.from(1))) == "Stream(Stream(1, 2, 3, 4, ...))")
      }
    }

    'set - {
      'empty - assert(inspect(Set.empty[Int]) == "Set()")
      'nonEmpty - assert(inspect(Set(1, 2, 3)) == "Set(1, 2, 3)")
    }

    'map - {
      'empty - assert(inspect(Map.empty[Int, Boolean]) == "Map()")
      'nonEmpty - assert(
        inspect(Map(1 -> true, 2 -> false)) == "Map(1 -> true, 2 -> false)")
    }

    'char - {
      'tab - assert(inspect('\t') == "'\\t'")
      'newline - assert(inspect('\n') == "'\\n'")
      'other - assert(inspect('a') == "'a'")
    }

    'string - assert(inspect("hello\tworld\r\n") == "\"hello\\tworld\\r\\n\"")

    'tiers - {
      'small - assert(
        inspect(Tiers(Seq(1, 2, 3), Seq(4, 5, 6))) == "Tiers(Seq(1, 2, 3), Seq(4, 5, 6))")
      'infinite - {
        'four - assert(
          inspect4(Tiers.fromList(Stream.from(1))) == "Tiers(Seq(1), Seq(2), Seq(3), Seq(4), ...)")
        'eight - assert(
          inspect(Tiers.fromList(Stream.from(1))) == "Tiers(Seq(1), Seq(2), Seq(3), Seq(4), Seq(5), Seq(6), Seq(7), Seq(8), ...)")
      }
    }

    'function - {
      'function0 - assert(inspect[() => Int](() => 42) == "(() => 42)")

      'function1 - {
        'identity - {
          val actual = inspect[Boolean => Boolean](x => x)
          val expected =
            """(x => x match {
              |  case true  => true
              |  case false => false
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'and - {
          val actual = inspect((x: Boolean) => (y: Boolean) => x && y)
          val expected =
            """(x => y => (x, y) match {
              |  case (true, true) => true
              |  case _            => false
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'or - {
          val actual = inspect((x: Boolean) => (y: Boolean) => x || y)
          val expected =
            """(x => y => (x, y) match {
              |  case (false, false) => false
              |  case _              => true
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'const - {
          val actual   = inspect[Int => Int](_ => 42)
          val expected = "(_ => 42)"
          assert(actual == expected)
        }

        'head - {
          val actual = inspect[List[Int] => Int](_.head)
          val expected =
            """(x => x match {
              |  case List()        => throw new NoSuchElementException("head of empty list")
              |  case List(0)       => 0
              |  case List(0, 0)    => 0
              |  case List(1)       => 1
              |  case List(0, 0, 0) => 0
              |  case List(0, 1)    => 0
              |  case List(1, 0)    => 1
              |  case List(-1)      => -1
              |  ...
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'boolFunc - {
          val actual = inspect { (f: Boolean => Boolean) => (x: Boolean) =>
            f(x)
          }
          val expected =
            """(x => y => (x, y) match {
              |  case ((x => x match { case true => false; case false => true }), true)  => false
              |  case ((x => x match { case true => true; case false => false }), false) => false
              |  case ((_ => false), _)                                                  => false
              |  case _                                                                  => true
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }
      }

      'function2 - {
        'and - {
          val actual = inspect((x: Boolean, y: Boolean) => x && y)
          val expected =
            """((x, y) => (x, y) match {
                |  case (true, true) => true
                |  case _            => false
                |})
                |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'or - {
          val actual = inspect((x: Boolean, y: Boolean) => x || y)
          val expected =
            """((x, y) => (x, y) match {
              |  case (false, false) => false
              |  case _              => true
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }

        'plus - {
          val actual = inspect((x: Int, y: Int) => x + y)
          val expected =
            """((x, y) => (x, y) match {
              |  case (0, 0)  => 0
              |  case (0, 1)  => 1
              |  case (1, 0)  => 1
              |  case (0, -1) => -1
              |  case (1, 1)  => 2
              |  case (-1, 0) => -1
              |  case (0, 2)  => 2
              |  case (1, -1) => 0
              |  ...
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }
      }

      'function3 - {
        'andOr - {
          val actual =
            inspect((x: Boolean, y: Boolean, z: Boolean) => x && y || z)
          val expected =
            """((x, y, z) => (x, y, z) match {
              |  case (_, _, true)    => true
              |  case (true, true, _) => true
              |  case _               => false
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }
      }

      'function4 - {
        'andOrAnd - {
          val actual =
            inspect((x: Boolean, y: Boolean, z: Boolean, w: Boolean) =>
              x && y || z && w)
          val expected =
            """((x, y, z, w) => (x, y, z, w) match {
              |  case (_, _, true, true) => true
              |  case (true, true, _, _) => true
              |  case _                  => false
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }
      }

      'function5 - {
        'andOrAndOr - {
          val actual =
            inspect(
              (x: Boolean, y: Boolean, z: Boolean, w: Boolean, x1: Boolean) =>
                x && y || z && w || x1)
          val expected =
            """((x, y, z, w, x1) => (x, y, z, w, x1) match {
              |  case (_, _, _, _, true)    => true
              |  case (_, _, true, true, _) => true
              |  case (true, true, _, _, _) => true
              |  case _                     => false
              |})
              |""".stripMargin.stripLineEnd
          assert(actual == expected)
        }
      }
    }
  }
}
