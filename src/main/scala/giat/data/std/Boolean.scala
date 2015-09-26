package giat
package data
package std

import giat.typeclass._

trait BooleanInstances {

  protected val any: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a: Boolean, b: => Boolean): Boolean = a || b
  }

  protected val all: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a: Boolean, b: => Boolean): Boolean = a && b
  }

}


object Boolean extends BooleanInstances { self =>

  object monoid {
    implicit val any: Monoid[Boolean] = self.any
    implicit val all: Monoid[Boolean] = self.all
  }
  object semigroup {
    implicit val any: Semigroup[Boolean] = self.any
    implicit val all: Semigroup[Boolean] = self.all
  }

}