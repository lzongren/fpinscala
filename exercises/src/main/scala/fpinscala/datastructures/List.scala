package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case neg if (neg < 0) => l
    case _ => l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)( (a, b) => b + 1 )

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /** Exercise 7 */
  // No, because foldRight will traverse down to bottom first then calculate.

  /** Exercise 8 */
  foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

  /** Exercise 11 */
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1: Double)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  /** Exercise 12 */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])( (b, a) => Cons(a, b) )

  /** Exercise 13 */
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  /** Exercise 14 */
  def append1(l: List[Int], e: Int): List[Int] = foldRight(l, Cons(e, Nil))(Cons(_, _))


  /** Exercise 15 */
  def flatten[A](ls: List[List[A]]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => foldLeft(xs, x)((b, a) => append(b, a))
  }

  /** Exercise 16 */
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  /** Exercise 17 */
  def toString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, toString(xs))
  }


  /** Exercise 18 */
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }


  /** Exercise 19 */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldLeft(l, Nil: List[A]) {
    (b, a) =>
      if (f(a)) Cons(a, b)
      else b
  }

  /** Exercise 20 */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldLeft(l, Nil: List[B]) {
    (b, a) => append(b, f(a))
  }

  /** Exercise 21 */
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) {
    (a: A) =>
      if (f(a)) Cons(a, Nil)
      else Nil
  }

  /** Exercise 22 */
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
  }

  /** Exercise 23 */
  def pairWise[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), pairWise(xs, ys)(f))
  }


}
