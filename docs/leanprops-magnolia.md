---
id: leanprops-magnolia
title: leanprops-magnolia
---

`leanprops-magnolia` is [Magnolia] integration to LeanProps.

<!--

# Installation

> TODO: LeanProps is not published for now....

Add this line into your `build.sbt`:

```scala
libraryDependencies += "codes.quine" % "leanprops-magnolia" % "@VERSION@"
```

-->

# Usage

`import codes.quine.leanprops.magnolia._` is needed to derive `Listable` and `Inspectable` instances.

```scala mdoc
import codes.quine.leanprops._, magnolia._

// Defines `Tree` ADT.
sealed trait Tree[A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Then, `Listable` and `Inspectable` instance for `Tree[A]` is derived by `leanprops-magnolia`.

Listable.list[Tree[Int]].take(10)
print(inspect(Branch(Leaf(1), Leaf(2))))
```

[Magnolia]: https://propensive.com/opensource/magnolia/