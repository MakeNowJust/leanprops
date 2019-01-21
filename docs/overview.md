---
id: overview
title: Overview
---

LeanProps is a simple enumerative property-based testing library.

Some features and concepts is ported from Haskell's [LeanCheck][] library.

## Warning

> This project is under development. It has no stable version for now, and some API might be changed.

## Features

1. Lists all possible values in respect to a type.
2. `inspect` - better `toString`. It can show a function value, powered by listing.
3. Tests a property. When a counter-example of the property is found, LeanProps shows it with `inspect`.

## Modules

### `leanprops-core`

`lesnprops-core` is core of this library.

It provides some important classes and traits: `Tiers[A]`, `Listable[A]` and `Inspectable[A]`.

See [details](./leanprops-core.md).

### `leanprops-magnolia`

`leanprops-magnolia` is [Magnolia] integration to LeanProps.

It provides `Listable` and `Inspectable` instances for any `T` powered by Magnolia.
You can use your type in properties for free.

See [details](./leanprops-magnolia.md).

[leancheck]: https://github.com/rudymatela/leancheck
[magnolia]: https://propensive.com/opensource/magnolia/
