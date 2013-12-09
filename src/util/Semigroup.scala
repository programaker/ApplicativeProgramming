package util

// A = Anything
trait Semigroup[A] {
  def append(v1: A, v2: A): A
}

object Semigroup {
  implicit def ListSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(v1: List[A], v2: List[A]) = v1 ::: v2
  }
}