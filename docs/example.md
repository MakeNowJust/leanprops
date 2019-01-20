---
id: example
title: One-Minute Example
---

To run this example, you should [install](./installation.md) `leanprops-core` to your project.

```scala mdoc
import codes.quine.leanprops._

// A small utility function to `inspect` a value in one-line.
def inspect4[A: Inspectable](x: A): String =
  Inspectable[A].inspect(x).run(InspectConfig.FourCases)


// ===========================================================================================
//
// Feature 1. Lists all possible values in respect to a type.

// Gets all possible `Int` values.
Listable.list[Int].take(10)

// Gets all possible `List[Int]` values.
Listable.list[List[Int]].take(10)

// Gets all possible `(Int, Int) => Int` values.
Listable.list[(Int, Int) => Int].take(10).map(inspect4[(Int, Int) => Int])


// ===========================================================================================
//
// Feature 2. `inspect` - better `toString`.

// Inspects an infinite `Stream[Int]`
println(inspect(Stream.from(1)))

// Inspects `&&` function.
println(inspect { (x: Boolean, y: Boolean) => x && y })

// Inspects `_.head` function.
println(inspect { (xs: List[Int]) => xs.head })


// ===========================================================================================
//
// Feature 3. Tests a property.

// Checks `x * 0 == 0` against first 100-elements of the above list.
holds(100) { (x: Int) => x * 0 == 0 }

// Above is the same as:
Listable.list[Int].take(100).forall { x => x * 0 == 0 }

// Finds an example that makes a difference between `foldLeft` and `foldRight`.
counterExample(1000) { (xs: List[Int], x: Int, f: (Int, Int) => Int) =>
  xs.foldLeft(x)(f) == xs.foldRight(x)(f)
}
```
