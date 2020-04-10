package tofu

import cats.Functor

/** set of typeclasses for binary type constructors
  * reflecting constraints for unary type constructors
  */
trait BiTypes {
  type NoError

  type UFunctor[F[_, _]] = Functor[F[NoError, *]]
}
