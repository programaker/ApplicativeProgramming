package domain

import util.Semigroup

// F = Failure
// S = Success
// R = Result
sealed trait Validation[F, S] {
  // functor - applies a function to an wrapped value and returns an wrapped result:
  // >>> map(V => R, Wrapped[V]) = Wrapped[R]
  // but in this case, object-oriented modeled, the wrapped value is the object itself:
  // >>> Wrapped[V].map(V => R) = Wrapped[R]
  def map[R](function: S => R): Validation[F, R] = this match {
    case Success(s) => Success(function(s))
    case Failure(f) => Failure(f)
  }

  // applicative functor - applies an wrapped function to an wrapped value and returns an wrapped result:
  // >>> apply(Wrapped[V => R], Wrapped[V]) = Wrapped[R]
  // but in this case, object-oriented modeled, the wrapped value is the object itself:
  // >>> Wrapped[V].apply(Wrapped[V => R]) = Wrapped[R]
  def apply[R](validation: Validation[F, S => R])(implicit failureAccumulator: Semigroup[F]): Validation[F, R] = {
    (this, validation) match {
      case (Failure(f1), Failure(f2)) => Failure(failureAccumulator.append(f1, f2))
      case (Failure(f), Success(_)) => Failure(f)
      case (Success(_), Failure(f)) => Failure(f)
      case (Success(s), Success(function)) => Success(function(s))
    }
  }
}

final case class Success[F, S](success: S) extends Validation[F, S]
final case class Failure[F, S](failure: F) extends Validation[F, S]