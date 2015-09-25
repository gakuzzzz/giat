package giat.data

sealed abstract class Ordering {

}
object Ordering {

  case object LT extends Ordering
  case object EQ extends Ordering
  case object GT extends Ordering

}

trait OrderingInstances {



}