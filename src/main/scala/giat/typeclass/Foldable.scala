package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Foldable[F[_]] {
  type FoldMapGuard

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

}
