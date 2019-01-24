package leanprops
package magnolia

trait Perspective[A] {
  def listable: Listable[A]
  def inspectable: Inspectable[A]
}

object Perspective extends PerspectiveImplicits {
  def apply[A: Perspective]: Perspective[A] = implicitly[Perspective[A]]
}

private[magnolia] trait PerspectiveImplicits {
  implicit def ListableInspectablePerspective[A: Listable: Inspectable]
    : Perspective[A] =
    new Perspective[A] {
      val listable: Listable[A]       = Listable[A]
      val inspectable: Inspectable[A] = Inspectable[A]
    }
  implicit def PerspectiveListable[A: Perspective]: Listable[A] =
    Listable.from(Tiers.reset(Perspective[A].listable.tiers))
  implicit def PerspectiveInspectable[A: Perspective]: Inspectable[A] =
    Perspective[A].inspectable
}
