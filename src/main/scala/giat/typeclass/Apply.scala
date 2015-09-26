package giat
package typeclass

import simulacrum._

@typeclass
trait Apply[F[_]] extends Functor[F] { self =>

  def ap[A, B](fa: => F[A])(ff: => F[A => B]): F[B]


  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap(fb)(map(fa)(f.curried))

  protected trait ApplySemigroup[A] extends Semigroup[F[A]] {
    implicit def A: Semigroup[A]
    import Semigroup.ops._
    override def op(a: F[A], b: => F[A]): F[A] = self.apply2(a, b)(_ |+| _)
  }

  def semigroup[A](implicit sa: Semigroup[A]): Semigroup[F[A]] = new ApplySemigroup[A] {
    implicit val A: Semigroup[A] = sa
  }

}
