package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _}

// hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  /** Exercise 4.6 */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case _ => _
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case _ => _
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (_, Left(x)) => Left(x)
      case (Left(x), _) => Left(x)
    }
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  /** Exercise 4.7 */
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case x :: xs => xs.foldLeft(f(x).map( _ :: Nil) ) {
        (acc, a) => for {
          ac <- acc
          aa <- f(a)
        } yield aa :: ac
      }
    }

  /** Exercise 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity(_))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}