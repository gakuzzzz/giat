package giat
package typeclass

import simulacrum._

@typeclass
trait Semigroup[A] { self =>

  @op("|+|")
  def op(a: A, b: => A): A

  protected trait SemigroupApply extends Apply[λ[α => A]] {
    override def map[AA, B](fa: A)(f: AA => B): A = fa
    def ap[AA, B](fa: => A)(f: => A) = self.op(f, fa)
  }

  lazy val apply: Apply[λ[α => A]] = new SemigroupApply {}

}
