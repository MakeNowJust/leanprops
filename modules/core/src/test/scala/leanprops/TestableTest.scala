package leanprops

import utest._

object TestableTest extends TestSuite {
  val tests = Tests {
    'boolean - {
      'true - assert(holds(1)(true))
      'false - assert(fails(1)(false))
    }

    'function0 - {
      'true - assert(holds(1)(() => true))
      'false - assert(fails(1)(() => false))
    }

    'function1 - {
      'identity - assert(counterExample(10)((x: Boolean) => x).contains(Seq(Seq("false"))))
      'not - assert(counterExample(10)((x: Boolean) => !x).contains(Seq(Seq("true"))))
    }

    'function2 - assert(
      counterExample(10)((x: Boolean, y: Boolean) => x && y)
        .contains(Seq(Seq("true", "false"))))

    'function3 - assert(
      counterExample(10)((x: Boolean, y: Boolean, z: Boolean) => x && y && z)
        .contains(Seq(Seq("true", "true", "false"))))

    'function4 - assert(
      counterExample(10)((x: Boolean, y: Boolean, z: Boolean, w: Boolean) => x && y && z && w)
        .contains(Seq(Seq("true", "true", "true", "false"))))

    'function5 - assert(
      counterExample(10)((x: Boolean, y: Boolean, z: Boolean, w: Boolean, x1: Boolean) => x && y && z && w && x1)
        .contains(Seq(Seq("true", "true", "true", "true", "false"))))
  }
}
