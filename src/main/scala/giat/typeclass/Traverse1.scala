package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Traverse1[F[_]] extends Traverse[F] with Foldable1[F] {

  def traverse1[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = traverse1(fa)(f)

  override def foldMap1[A, B: Semigroup](fa: F[A])(f: A => B): B = {
    traverse1[λ[α => B], A, B](fa)(f)(Semigroup[B].apply)
  }

}
