package leanprops
package cats

import _root_.cats._

// TODO: write test for `WithInspectConfig` instances.

trait CatsWithInspectConfig {
  implicit val catsWithInspectConfigInstances: Monad[WithInspectConfig] = new Monad[WithInspectConfig] {
    def tailRecM[A, B](x: A)(f: A => WithInspectConfig[Either[A, B]]): WithInspectConfig[B] = {
      def loop(x: A): WithInspectConfig[B] = f(x).flatMap {
        case Left(a)  => loop(a)
        case Right(b) => WithInspectConfig.pure(b)
      }
      loop(x)
    }
    def flatMap[A, B](w: WithInspectConfig[A])(f: A => WithInspectConfig[B]): WithInspectConfig[B] = w.flatMap(f)
    def pure[A](x: A): WithInspectConfig[A]                                                        = WithInspectConfig.pure(x)
  }
}

object CatsWithInspectConfig extends CatsWithInspectConfig
