package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Applicative[F[_]] extends Apply[F] { self =>
  override type MapGuard <: Applicative[F]

  def point[A](a: => A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(point(f))

  protected trait ApplicativeMonoid[A] extends ApplySemigroup[A] with Monoid[F[A]] {
    implicit def A: Monoid[A]
    override def zero: F[A] = self.point(A.zero)
  }

  def monoid[A](implicit ma: Monoid[A]): Monoid[F[A]] = new ApplicativeMonoid[A] {
    implicit val A: Monoid[A] = ma
  }

}
