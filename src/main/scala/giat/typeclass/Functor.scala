package giat
package typeclass

import simulacrum._

@typeclass
trait Functor[F[_]] {
  type MapGuard <: Functor[F]

  def map[A, B](fa: F[A])(f: A => B): F[B]

}
