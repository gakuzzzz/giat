package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Foldable1[F[_]] extends Foldable[F] with LinearizeGuardian {
  override type Guard <: Foldable1[F]

  def foldMap1[A, B: Semigroup](fa: F[A])(f: A => B): B

  override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = foldMap1(fa)(f)

}
