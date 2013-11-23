package util

// A = Anything
trait Monoid[A] {
  def identity: A

  // Semigroup
  def append(v1: A, v2: A): A
}

object Monoid {
  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def identity = List()
    def append(v1: List[A], v2: List[A]) = v1 ::: v2
  }
}