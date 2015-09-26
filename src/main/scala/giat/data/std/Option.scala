package giat
package data
package std

import giat.typeclass._

trait OptionInstances {

  protected val default: Traverse[Option] with Monad[Option] = new Traverse[Option] with Monad[Option] {
    override def point[A](a: => A): Option[A] = Some(a)
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case Some(a) => Applicative[G].map(f(a))(Some.apply)
      case None    => Applicative[G].point(None)
    }
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  protected def first[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero = None
    override def op(a: Option[A], b: => Option[A]) = a orElse b
  }

  protected def last[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero = None
    override def op(a: Option[A], b: => Option[A]) = b orElse a
  }

  protected def delegate[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    import Semigroup.ops._
    override def zero = None
    override def op(a: Option[A], b: => Option[A]) = default.apply2(a, b)(_ |+| _)
  }

}

object Option extends OptionInstances { self =>

  object traverse    { implicit val default: Traverse[Option] = self.default }
  object foldable    { implicit val default: Foldable[Option] = self.default }
  object monad       { implicit val default: Monad[Option] = self.default }
  object applicative { implicit val default: Applicative[Option] = self.default }
  object bind        { implicit val default: Bind[Option] = self.default }
  object apply       { implicit val default: Apply[Option] = self.default }
  object functor     { implicit val default: Functor[Option] = self.default }

  object monoid {
    implicit def first[A]: Monoid[Option[A]] = self.first[A]
    implicit def last[A]: Monoid[Option[A]] = self.last[A]
    implicit def delegate[A: Semigroup]: Monoid[Option[A]] = self.delegate
    implicit def applicative[A: Monoid]: Monoid[Option[A]] = self.applicative.default.monoid
  }

  object semigroup {
    implicit def first[A]: Semigroup[Option[A]] = self.first[A]
    implicit def last[A]: Semigroup[Option[A]] = self.last[A]
    implicit def delegate[A: Semigroup]: Semigroup[Option[A]] = self.delegate
    implicit def apply[A: Semigroup]: Semigroup[Option[A]] = self.applicative.default.semigroup
  }

}



