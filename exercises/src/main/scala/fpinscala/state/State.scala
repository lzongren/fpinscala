package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (v, r) = rng.nextInt
    (if (v % 2 == 0) true else false, r)
  }


  /** Exercise 1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    (if (v >= 0) v else (v + 1).abs, r)
  }


  /** Exercise 2 */
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v / Int.MaxValue.toDouble, r)
  }


  /** Exercise 3 */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ( (i, d), r2 )
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ( (d, i), r2 )
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ( (d1, d2, d3), r3 )
  }


  /** Exercise 4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (is, rs) = ints(count - 1)(r)
      (i :: is, rs)
    }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {

    def go(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) =
      if (count <= 0) (l, rng)
      else {
        val (v, r) = rng.nextInt
        go(count - 1, r, v :: l)
      }

    go(count, rng, Nil)
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = map(nonNegativeInt) (_ / n)

  /** Exercise 5 */
  def double1: Rand[Double] = map(nonNegativeInt)( _ / (Int.MaxValue + 1))


  /** Exercise 6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  /** Exercise 7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft((Nil: List[A], rng))( (acc, f) => {
      val (l, rng) = acc
      val (a, nextRng) = f(rng)
      (a +: l, nextRng)
    }  )
  }

  /** Exercise 8 */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    g(a)(rngA)
  }

  /** Exercise 9 */
  def mapUsingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)( a => (rng => (f(a), rng) ) )


  def map2UsingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)( a => (rng => {
      val (b, rngB) = rb(rng)
      (f(a, b), rngB)
    } ) )


}

case class State[S,+A](run: S => (A, S)) {

  /** Exercise 10 */
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, sA) = run(s)
    (f(a), sA)
  })


  /** Exercise 10 */
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, stateA) = run(s)
    val (b, stateB) = sb.run(stateA)
    (f(a, b), stateB)
  })

  /** Exercise 10 */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, stateA) = run(s)
    f(a).run(stateA)
  })

}

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State( machine => {
    val last = inputs.foldLeft( machine ) {
      (prev: Machine, input) =>
        (prev.candies, prev.locked, input) match {
          case (0, _, _) => prev
          case (_, false, Coin) => Machine(false, prev.candies, prev.coins + 1)
          case (_, false, Turn) => Machine(true, prev.candies - 1, prev.coins)
          case (_, true, Coin) => Machine(false, prev.candies, prev.coins + 1)
          case (_, true, Turn) => prev
        }
    }
    ((last.coins, last.candies), last)
  })

  /** Exercise 10 */
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))


  /** Exercise 10 */
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = State(s =>
    l.foldLeft( (Nil: List[A], s: S) ) {
      (acc, x) => {
        val (accS: S, accL: List[A]) = acc
        val next = x.run(accS)
        val (nextA, nextS) = next
        (nextA :: accL, nextS)
      }
  })

  /** Exercise 12 */
  def get[S]: State[S, S] = State(s => (s, s))

  /** Exercise 12 */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
