package giat
package typeclass

import simulacrum.typeclass

@typeclass
trait Monad[F[_]] extends Applicative[F] with Bind[F] {

}
