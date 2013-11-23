package domain

import util.Monoid

// F = Failure
// S = Success
// R = Result
sealed trait Validation[F, S] {
  // functor - applies a function to an wrapped value and returns another wrapped value
  // Wrapper[V].map(V => R) = Wrapper[R]
  def map[R](function: S => R): Validation[F, R] = this match {
    case Success(s) => Success(function(s))
    case Failure(f) => Failure(f)
  }

  // applicative functor - applies an wrapped function to an wrapped value and returns another wrapped value
  // Wrapper[V].apply(Wrapper[V => R]) = Wrapper[R]
  // in this case, object-oriented modeled, the wrapped value is the object itself,
  // not an argument of a pure function
  def apply[R](validation: Validation[F, S => R])(implicit failureAccumulator: Monoid[F]): Validation[F, R] = {
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