package giat
package typeclass

import giat.data.Id

import simulacrum.typeclass

@typeclass
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  override type Guard <: LinearizeGuardian

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = {
    traversal[λ[α => B]](Monoid[B].applicative).run(fa)(f)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traversal(Id.applicative.default).run(fa)(f)

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  class Traversal[G[_]: Applicative] {
    def run[A, B](fa: F[A])(f: A => G[B]): G[F[B]] = self.traverse(fa)(f)
  }

  def traversal[G[_]: Applicative]: Traversal[G] = new Traversal[G]



}
