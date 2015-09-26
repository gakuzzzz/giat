package giat
package data

import giat.typeclass._

trait Function1Definition {

  type Kleisli[F[_], A, B] = A => F[B]
  type ReaderT[F[_], E, A] = Kleisli[F, E, A]
  type Reader[E, A] = ReaderT[Id, E, A]

  type IndexedContsT[W[_], M[_], R, O, A] = W[A => M[O]] => M[R]

  type IndexedStateT[F[_], -S1, S2, A] = S1 => F[(S2, A)]
  type IndexedState[-S1, S2, A] = IndexedStateT[Id, S1, S2, A]
  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  type State[S, A] = StateT[Id, S, A]

  type Endo[A] = A => A

}

trait Function1Instances {

  protected trait KleisliFunctor[F[_], E] extends Functor[Kleisli[F, E, ?]] {
    implicit def F: Functor[F]
    override def map[A, B](fa: Kleisli[F, E, A])(f: A => B): Kleisli[F, E, B] = { e => F.map(fa(e))(f) }
  }

  protected trait KleisliApply[F[_], E] extends Apply[Kleisli[F, E, ?]] with KleisliFunctor[F, E] {
    override implicit def F: Apply[F]
    override def ap[A, B](fa: => Kleisli[F, E, A])(f: => Kleisli[F, E, A => B]): Kleisli[F, E, B] = { e =>
      F.ap(fa(e))(f(e))
    }
  }

  protected trait KleisliApplicative[F[_], E] extends Applicative[Kleisli[F, E, ?]] with KleisliApply[F, E] {
    override implicit def F: Applicative[F]
    override def point[A](a: => A): Kleisli[F, E, A] = { e => F.point(a) }
  }

  protected trait KleisliBind[F[_], E] extends Bind[Kleisli[F, E, ?]] with KleisliApply[F, E] {
    override implicit def F: Bind[F]
    override def flatMap[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]): Kleisli[F, E, B] = { e =>
      F.flatMap(fa(e)) { f(_)(e) }
    }
  }

  protected trait KleisliMonad[F[_], E] extends Monad[Kleisli[F, E, ?]] with KleisliApplicative[F, E] with KleisliBind[F, E] {
    override implicit def F: Monad[F]
  }

  protected def kleisliFunctor[F[_], E](implicit mf: Functor[F]): Functor[Kleisli[F, E, ?]] = new KleisliFunctor[F, E] {
    override implicit def F = mf
  }

  protected def kleisliApply[F[_], E](implicit mf: Apply[F]): Apply[Kleisli[F, E, ?]] = new KleisliApply[F, E] {
    override implicit def F = mf
  }

  protected def kleisliApplicative[F[_], E](implicit mf: Applicative[F]): Applicative[Kleisli[F, E, ?]] = new KleisliApplicative[F, E] {
    override implicit def F = mf
  }

  protected def kleisliBind[F[_], E](implicit mf: Bind[F]): Bind[Kleisli[F, E, ?]] = new KleisliBind[F, E] {
    override implicit def F = mf
  }

  protected def kleisliMonad[F[_], E](implicit mf: Monad[F]): Monad[Kleisli[F, E, ?]] = new KleisliMonad[F, E] {
    override implicit def F = mf
  }

  protected def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    override def point[A](a: => A): State[S, A] = s => (s, a)
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = { s =>
      val (s1, a) = fa(s)
      f(a)(s1)
    }
  }

  protected def endoMonoid[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    override def op(a: Endo[A], b: => Endo[A]): Endo[A] = a.compose(b)
    override val zero: Endo[A] = identity
  }

}

object StdFunction1 extends Function1Instances { self =>

  import Id.monad.default

  object monad {
    implicit def kleisli[F[_]: Monad, E]: Monad[Kleisli[F, E, ?]] = self.kleisliMonad[F, E]
    implicit def readerT[F[_]: Monad, E]: Monad[ReaderT[F, E, ?]] = self.kleisliMonad[F, E]
    implicit def reader[E]: Monad[Reader[E, ?]] = kleisli[Id, E]
    implicit def state[S]: Monad[State[S, ?]] = self.stateMonad[S]
  }
  object bind {
    implicit def kleisli[F[_]: Bind, E]: Bind[Kleisli[F, E, ?]] = self.kleisliBind[F, E]
    implicit def readerT[F[_]: Bind, E]: Bind[ReaderT[F, E, ?]] = self.kleisliBind[F, E]
    implicit def reader[E]: Bind[Reader[E, ?]] = kleisli[Id, E]
    implicit def state[S]: Bind[State[S, ?]] = self.stateMonad[S]
  }
  object applicative {
    implicit def kleisli[F[_]: Applicative, E]: Applicative[Kleisli[F, E, ?]] = self.kleisliApplicative[F, E]
    implicit def readerT[F[_]: Applicative, E]: Applicative[ReaderT[F, E, ?]] = self.kleisliApplicative[F, E]
    implicit def reader[E]: Applicative[Reader[E, ?]] = kleisli[Id, E]
    implicit def state[S]: Applicative[State[S, ?]] = self.stateMonad[S]
  }
  object apply {
    implicit def kleisli[F[_]: Apply, E]: Apply[Kleisli[F, E, ?]] = self.kleisliApply[F, E]
    implicit def readerT[F[_]: Apply, E]: Apply[ReaderT[F, E, ?]] = self.kleisliApply[F, E]
    implicit def reader[E]: Apply[Reader[E, ?]] = kleisli[Id, E]
    implicit def state[S]: Apply[State[S, ?]] = self.stateMonad[S]
  }
  object functor {
    implicit def kleisli[F[_]: Functor, E]: Functor[Kleisli[F, E, ?]] = self.kleisliFunctor[F, E]
    implicit def readerT[F[_]: Functor, E]: Functor[ReaderT[F, E, ?]] = self.kleisliFunctor[F, E]
    implicit def reader[E]: Functor[Reader[E, ?]] = kleisli[Id, E]
    implicit def state[S]: Functor[State[S, ?]] = self.stateMonad[S]
  }

  object monoid {
    implicit def endo[A]: Monoid[Endo[A]] = self.endoMonoid[A]
  }

}