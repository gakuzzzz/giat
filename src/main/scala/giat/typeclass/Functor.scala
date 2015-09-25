package giat
package typeclass

import simulacrum._

@typeclass
trait Functor[F[_]] extends LinearizeGuardian {
  override type Guard <: LinearizeGuardian

  def map[A, B](fa: F[A])(f: A => B): F[B]

}
