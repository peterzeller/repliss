package crdtver.utils

import crdtver.Repliss.Error



sealed abstract class Result[+T, +E] {


  def map[S](f: T => S): Result[S, E] = this match {
    case Ok(value) => Ok(f(value))
    case Err(errors) => Err(errors)
  }

  def flatMap[S, E2 >: E](f: T => Result[S, E2]): Result[S, E2] = this match {
    case Ok(value) => f(value)
    case Err(errors) => Err(errors)
  }


  def foreach(fn: T => Unit): Unit = this match {
    case Ok(value) => fn(value)
    case Err(errors) =>
  }


}

case class Ok[+T, +E](
  value: T
) extends Result[T, E] {
  def hasErrors = false

  def get(): T = value
}

case class Err[+E, +T](
  error: T
) extends Result[E, T] {
  def hasErrors = true

}

