package giat
package typeclass

import simulacrum._

@typeclass
trait Monoid[A] extends Semigroup[A] { self =>

  def zero: A

  protected trait MonoidApplicative extends Applicative[λ[α => A]] with SemigroupApply {
    def point[AA](a: => AA): A = self.zero
  }

  lazy val applicative: Applicative[λ[α => A]] = new MonoidApplicative {}

}
