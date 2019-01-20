---
id: leanprops-core
title: leanprops-core
---

`leanprops-core` is core of LeanProps.

It provides some important classes and traits: `Tiers[A]`, `Listable[A]` and `Inspectable[A]`.

<!--

# Installation

> TODO: LeanProps is not published for now....

Add this line into your `build.sbt`:

```scala
libraryDependencies += "codes.quine" % "leanprops-core" % "@VERSION@"
```

-->

# Usage

All examples need to import `codes.quine.leanprops` of course:

```scala mdoc
import codes.quine.leanprops._
```

When you have a property `p` that is a function returns `Boolean` value, then `holds(100)(p)` tests `p` against first 100 possible arguments.

```scala mdoc
val p: Int => Boolean = { x => x == x }
holds(100)(p)

// Or, passes a property to `holds` as function literal.
holds(100) { (x: Int) => x == x }
```

`counterExample(100)(p)` finds arguments that not satisify `p`. Such arguments are called as a counter example. When a counter example is found, it returns `Some(...)` with `inspect`-ed arguments, otherwise it returns `None`.

```scala mdoc
counterExample(100) { (x: Int) => x != x }

counterExample(100) { (x: Int) => x == x }
```
