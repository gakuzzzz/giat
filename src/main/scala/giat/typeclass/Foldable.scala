package giat
package typeclass

import simulacrum.typeclass
import giat.data._

@typeclass
trait Foldable[F[_]] {
  type FoldMapGuard

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B


  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def length[A](fa: F[A]): Int = foldMap(fa)(_ => 1)(std.Int.monoid.sum)

  def all[A](fa: F[A])(f: A => Boolean): Boolean = foldMap(fa)(f)(std.Boolean.monoid.all)

  def any[A](fa: F[A])(f: A => Boolean): Boolean = foldMap(fa)(f)(std.Boolean.monoid.any)


}
