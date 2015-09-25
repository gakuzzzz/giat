package giat
package data
package std

import giat.typeclass._

trait EitherInstances {

  protected def right[L]: Traverse[Either[L, ?]] with Monad[Either[L, ?]] = new Traverse[Either[L, ?]] with Monad[Either[L, ?]] {
    override def point[A](a: => A): Either[L, A] = Right(a)
    override def traverse[G[_]: Applicative, A, B](fa: Either[L, A])(f: A => G[B]): G[Either[L, B]] = fa match {
      case Left(x)  => Applicative[G].point(Left(x))
      case Right(x) => Applicative[G].map(f(x))(Right.apply)
    }
    override def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa.right.flatMap(f)
    override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.right.map(f)
  }

  protected def left[R]: Traverse[Either[?, R]] with Monad[Either[?, R]] = new Traverse[Either[?, R]] with Monad[Either[?, R]] {
    override def point[A](a: => A): Either[A, R] = Left(a)
    override def traverse[G[_]: Applicative, A, B](fa: Either[A, R])(f: A => G[B]): G[Either[B, R]] = fa match {
      case Left(x)  => Applicative[G].map(f(x))(Left.apply)
      case Right(x) => Applicative[G].point(Right(x))
    }
    override def flatMap[A, B](fa: Either[A, R])(f: A => Either[B, R]): Either[B, R] = fa.left.flatMap(f)
    override def map[A, B](fa: Either[A, R])(f: A => B): Either[B, R] = fa.left.map(f)
  }

  protected def validation[L: Semigroup]: Traverse[Either[L, ?]] with Applicative[Either[L, ?]] = new Traverse[Either[L, ?]] with Applicative[Either[L, ?]] {
    override def point[A](a: => A): Either[L, A] = Right(a)
    override def traverse[G[_]: Applicative, A, B](fa: Either[L, A])(f: A => G[B]): G[Either[L, B]] = fa match {
      case Left(x)  => Applicative[G].point(Left(x))
      case Right(x) => Applicative[G].map(f(x))(Right.apply)
    }
    import Semigroup.ops._
    override def ap[A, B](fa: => Either[L, A])(ff: => Either[L, A => B]): Either[L, B] = (fa, ff) match {
      case (Left(x), Left(y))   => Left(x |+| y)
      case (Right(a), Right(f)) => Right(f(a))
      case (Left(x), Right(_))  => Left(x)
      case (Right(_), Left(x))  => Left(x)
    }
  }

}


object Either extends EitherInstances { self =>

  object traverse {
    implicit def right[L]: Traverse[Either[L, ?]] = self.right[L]
    implicit def left[R]: Traverse[Either[?, R]] = self.left[R]
    implicit def validation[L: Semigroup] = self.validation[L]
  }
  object foldable {
    implicit def right[L]: Foldable[Either[L, ?]] = self.right[L]
    implicit def left[R]: Foldable[Either[?, R]] = self.left[R]
    implicit def validation[L: Semigroup] = self.validation[L]
  }
  object monad {
    implicit def right[L]: Monad[Either[L, ?]] = self.right[L]
    implicit def left[R]: Monad[Either[?, R]] = self.left[R]
  }
  object applicative {
    implicit def right[L]: Applicative[Either[L, ?]] = self.right[L]
    implicit def left[R]: Applicative[Either[?, R]] = self.left[R]
    implicit def validation[L: Semigroup] = self.validation[L]
  }
  object bind {
    implicit def right[L]: Bind[Either[L, ?]] = self.right[L]
    implicit def left[R]: Bind[Either[?, R]] = self.left[R]
  }
  object apply {
    implicit def right[L]: Apply[Either[L, ?]] = self.right[L]
    implicit def left[R]: Apply[Either[?, R]] = self.left[R]
    implicit def validation[L: Semigroup] = self.validation[L]
  }
  object functor {
    implicit def right[L]: Functor[Either[L, ?]] = self.right[L]
    implicit def left[R]: Functor[Either[?, R]] = self.left[R]
    implicit def validation[L: Semigroup] = self.validation[L]
  }


}