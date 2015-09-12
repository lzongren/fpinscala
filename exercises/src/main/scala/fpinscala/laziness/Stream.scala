package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }


  /** Exercise 1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xs) => x() :: xs().toList
  }

  /** Exercise 2 */
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
    case Empty => Empty
    case Cons(x, xs) => Stream.cons(x(), xs().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) => xs().drop(n - 1)
  }

  /** Exercise 3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) =>
      if (p(x())) Stream.cons(x(), xs().takeWhile(p))
      else Empty
  }

  /** Exercise 4 */
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)( (a, b) => p(a) && b )

  /** Exercise 5 */
  def takeWhile1(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A]) {
    (a, b) =>
      if (p(a)) Stream.cons(a, b)
      else Empty: Stream[A]
  }

  /** Exercise 7 */
  def map[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B]) {
    (a, b) => Stream.cons(f(a), b)
  }

  def filter(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A]) {
    (a, b) => if (p(a)) Stream.cons(a, b) else b
  }

  def append[B >: A](rest: => Stream[B]): Stream[B] = this.foldRight(rest) {
    (a, b) => Stream.cons(a, b)
  }

//  def flatMap

  /** Exercise 13 */
  def map1[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(x, xs) => Some((f(x()), xs()))
  }

  def take1(n: Int): Stream[A] = unfold((n, this)) {
    case (n, _) if n <= 0 => None
    case (_, Empty) => None
    case (n, Cons(x, xs)) => Some((x(), (n - 1, xs())))
  }

  def takeWhile2(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(x, xs) if p(x()) => Some((x(), xs()))
    case _ => None
  }

  def zip[B](that: Stream[B]): Stream[(A, B)] = unfold( (this, that)) {
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(x, xs), Cons(y, ys)) => Some( ((x(), y()), (xs(), ys())))
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, that)) {
    case (Cons(x, xs), Empty) => Some( (Some(x()), None), (xs(), Empty) )
    case (Empty, Cons(y, ys)) => Some( (None, Some(y())), (Empty, ys()) )
    case (Cons(x, xs), Cons(y, ys)) => Some( (Some(x()), Some(y())), (xs(), ys()) )
    case _ => None
  }


  /** Exercise 5.6 */
  def headOption: Option[A] = this.foldRight(None: Option[A]) {
    (a, acc) => Some(a)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.


  /** Exercise 14 */
  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(x, xs), Cons(y, ys)) if (x() == y()) => xs().startsWith(ys())
    case _ => false
  }


  /** Exercise 15 */
  def tails: Stream[Stream[A]] = this.foldRight(Stream.cons(Empty: Stream[A], Empty: Stream[Stream[A]])) {
    (a, b) => Stream.cons(Stream.cons(a, b.headOption.get), b)
  }


  /** Exercise 16 */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    val (_, bs: Stream[B]) = this.foldRight(z, Stream(z)) {
      case (a, Cons(b: B, bs: Stream[B])) => {
        lazy val nextZ = f(a, b)
        (nextZ, Stream.cons(nextZ, bs))
      }
    }
    bs
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)


  def range(from: Int, to: Int, step: Int): Stream[Int] =
    if (from + step > to) empty
    else cons(from, range(from + step, to, step))


  /** Exercise 8 */
  def constant[A](a: A): Stream[A] = {
    lazy val as = Stream.cons(a, as)
    as
  }

  /** Exercise 9 */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))


  /** Exercise 10 */
  def fibs: Stream[Int] = {
    def fib(a: Int, b: Int): Stream[Int] = Stream.cons(a, fib(b, a + b))
    fib(0, 1)
  }

  /** Exercise 11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  /** Exercise 12 */
  def fibs1: Stream[Int] = unfold( (0, 1) ) {
    case (a1, a2) => Some((a1 + a2, (a2, a1 + a2) ))
  }

  def from1(n: Int): Stream[Int] = unfold(n)( (s) => Some((s + 1, s + 1)) )

  def constant1[A](a: A): Stream[A] = unfold(a)( (_) => Some(a, a) )

  def ones1: Stream[Int] = unfold(1)( (_) => Some((1, 1)) )
}