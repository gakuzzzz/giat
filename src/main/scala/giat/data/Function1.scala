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

  protected def kleisliMonad[F[_], E](implicit mf: Monad[F]): Monad[Kleisli[F, E, ?]] = new Monad[Kleisli[F, E, ?]] {
    override def point[A](a: => A): Kleisli[F, E, A] = _ => mf.point(a)
    override def flatMap[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]): Kleisli[F, E, B] = { e: E =>
      mf.flatMap(fa(e)) { a: A => f(a)(e) }
    }
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
    implicit def reader[E]: Monad[Reader[E, ?]] = self.kleisliMonad[Id, E]
    implicit def state[S]: Monad[State[S, ?]] = self.stateMonad[S]
  }
  object bind {
    implicit def reader[E]: Bind[Reader[E, ?]] = self.kleisliMonad[Id, E]
    implicit def state[S]: Bind[State[S, ?]] = self.stateMonad[S]
  }
  object applicative {
    implicit def reader[E]: Applicative[Reader[E, ?]] = self.kleisliMonad[Id, E]
    implicit def state[S]: Applicative[State[S, ?]] = self.stateMonad[S]
  }
  object apply {
    implicit def reader[E]: Apply[Reader[E, ?]] = self.kleisliMonad[Id, E]
    implicit def state[S]: Apply[State[S, ?]] = self.stateMonad[S]
  }
  object functor {
    implicit def reader[E]: Functor[Reader[E, ?]] = self.kleisliMonad[Id, E]
    implicit def state[S]: Functor[State[S, ?]] = self.stateMonad[S]
  }

  object monoid {
    implicit def endo[A]: Monoid[Endo[A]] = self.endoMonoid[A]
  }

}