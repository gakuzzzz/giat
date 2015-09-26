package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Applicative[F[_]] extends Apply[F] {
  override type MapGuard <: Applicative[F]

  def point[A](a: => A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(point(f))

}
