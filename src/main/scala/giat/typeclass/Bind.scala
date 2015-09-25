package giat
package typeclass

import simulacrum._

@typeclass
trait Bind[F[_]] extends Apply[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: => F[A])(ff: => F[A => B]): F[B] = flatMap(ff)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}
