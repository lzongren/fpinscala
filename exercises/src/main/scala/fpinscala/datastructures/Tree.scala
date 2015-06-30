package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /** Exercise 25 */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /** Exercise 26 */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max (maximum(r))
  }

  /** Exercise 27 */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /** Exercise 28 */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /** Exercise 29 */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}