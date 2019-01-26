package leanprops

import _root_.magnolia.{CaseClass, Magnolia, SealedTrait}

package object magnolia extends PerspectiveImplicits {
  type Typeclass[T] = Perspective[T]

  def combine[T](ctx: CaseClass[Perspective, T]): Perspective[T] =
    new Perspective[T] {
      def listable: Listable[T] = Listable.from {
        Tiers
          .products(ctx.parameters.map(_.typeclass.listable.tiers): _*)
          .map(ctx.rawConstruct(_))
      }

      def inspectable: Inspectable[T] = Inspectable.fromInspect { v =>
        if (ctx.parameters.isEmpty) WithInspectConfig.pure(ctx.typeName.short)
        else {
          val ss =
            ctx.parameters.map(p => p.typeclass.inspectable.inspect(p.dereference(v)))
          WithInspectConfig
            .sequence(ss)
            .map(_.mkString(ctx.typeName.short ++ "(", ", ", ")"))
        }
      }
    }

  def dispatch[T](ctx: SealedTrait[Perspective, T]): Perspective[T] =
    new Perspective[T] {
      def listable: Listable[T] =
        Listable.from(Tiers.delay {
          ctx.subtypes
            .map(_.typeclass.listable.tiers.asInstanceOf[Tiers[T]])
            .foldLeft(Tiers.empty[T])(_ \++/ _)
        })

      def inspectable: Inspectable[T] = Inspectable.fromInspect { v =>
        ctx.dispatch(v) { sub =>
          sub.typeclass.inspectable.inspect(sub.cast(v))
        }
      }
    }

  implicit def gen[T]: Perspective[T] =
    macro Magnolia.gen[T]
}
