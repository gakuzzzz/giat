package giat
package data
package std

import typeclass._

trait IntInstances {

  protected lazy val sum: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a: Int, b: => Int): Int = a + b
  }

  protected lazy val product: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1
    override def op(a: Int, b: => Int): Int = a * b
  }

}

object Int extends IntInstances { self =>

  object monoid {
    implicit val sum: Monoid[Int] = self.sum
    implicit val product: Monoid[Int] = self.product
  }
  object semigroup {
    implicit val sum: Semigroup[Int] = self.sum
    implicit val product: Semigroup[Int] = self.product
  }

}