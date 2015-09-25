package giat.data

import giat.typeclass._

trait IdDefinition {

  type Id[+A] = A

}

trait IdInstances extends IdDefinition {

  protected val default: Traverse1[Id] with Monad[Id] = new Traverse1[Id] with Monad[Id] {
    override def point[A](a: => A): A = a
    override def traverse1[G[_]: Apply, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)
    override def flatMap[A, B](fa: A)(f: A => B): B = f(fa)
  }

}

object Id extends IdInstances { self =>

  object traverse1   { implicit val default: Traverse1[Id] = self.default }
  object traverse    { implicit val default: Traverse[Id] = self.default }
  object foldable1   { implicit val default: Foldable1[Id] = self.default }
  object foldable    { implicit val default: Foldable[Id] = self.default }
  object monad       { implicit val default: Monad[Id] = self.default }
  object applicative { implicit val default: Applicative[Id] = self.default }
  object bind        { implicit val default: Bind[Id] = self.default }
  object apply       { implicit val default: Apply[Id] = self.default }
  object functor     { implicit val default: Functor[Id] = self.default }

}