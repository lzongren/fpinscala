package fpinscala.parallelism

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  /** Exercise 3 */
  def timeoutMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /** Exercise 6 */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map
      asyncF( a => f(a) match {
        case true => a :: Nil
        case false => Nil
      })
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  /** Exercise 4 */
  def asyncF[A, B](f: A => B): A => Par[B] = a => fork(unit(f(a)))

  /**
   * Promotes a constant value to a parallel computation.
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  /**
   * Marks a computation for concurrent evaluation, won't actually occur until
   * forced by run.
   */
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
   * Lift any function of type `A => B` to a `Par[A] => Par[B]`
   */
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  /**
   * Combines the results of two parallel computations with a binary function.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  /** Exercise 5 */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A])) {
      (h: Par[A], acc: Par[List[A]]) => map2(h, acc)(_ :: _)
    }

  /**
   * Wrap unevaluated argument and marks it for concurrent evaluation.
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Exercise 6 extended */
  def words(ps: List[String]): Par[Int] = {
    val ws: Par[List[Int]] = parMap(ps)(_.split(' ').length)
    map(ws)(_.sum)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Exercise 6 extended */
  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])
                         (f: (A, B, C, D) => E): Par[E] = {
    val pab: Par[(A, B)] = map2(a, b)((_, _))
    val pcd: Par[(C, D)] = map2(c, d)((_, _))
    map2(pab, pcd)(
      (vab, vcd) => {
        val (va, vb) = vab
        val (vc, vd) = vcd
        f(va, vb, vc, vd)
      }
    )
  }

  /** Exercise 6 extended */
  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])
                            (f: (A, B, C, D, E) => F): Par[F] = {
    val pab: Par[(A, B)] = map2(a, b)((_, _))
    val pcde: Par[(C, D, E)] = map3(c, d, e)((_, _, _))

    map2(pab, pcde)(
      (vab, vcde) => {
        val (va, vb) = vab
        val (vc, vd, ve) = vcde
        f(va, vb, vc, vd, ve)
      }
    )
  }

  /** Exercise 6 extended */
  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val pab: Par[(A, B)] = map2(a, b)((_, _))
    map2(pab, c)(
      (vab, vc) => {
        val (va, vb) = vab
        f(va, vb, vc)
      }
    )
  }

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = es => {
    val a = fa(es)
    val b = fb(es)
    UnitFuture((a.get, b.get))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /** Exercise 11 */
  def choiceByChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(t :: f :: Nil)

  /** Exercise 11 */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val choice = choices(run(es)(n).get)
    run(es)(choice)
  }

  /** Exercise 12 */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val choice = choices(run(es)(key).get)
    run(es)(choice)
  }

  /** Exercise 13 */
  def choiceUsingChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  /** Exercise 13 */
  def choiceNUsingChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  /** Exercise 13 */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val choice = choices(run(es)(pa).get)
    run(es)(choice)
  }

  /** Exercise 14 */
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val inner = run(es)(a).get
    run(es)(inner)
  }

  /** Exercise 14 */
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  /**
   * Extract the value by actually performing the computation.
   */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
