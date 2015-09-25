package giat
package typeclass

import simulacrum._

@typeclass
trait Apply[F[_]] extends Functor[F] {

  def ap[A, B](fa: => F[A])(ff: => F[A => B]): F[B]

}
