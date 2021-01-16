package crdtver.utils

import cats.Monad
import cats.data.State

/**
 * Like Cats State monad but covariant in the result type
 */
case class CState[S, +T](
  step: S => (T, S)
) {
  def run(constraints: S): (S, T) = step(constraints).swap

}

object CState {
  def modify[S](f: S => S): CState[S, Unit] =
    CState(s => ((), f(s)))

  def pure[S, T](x: T): CState[S, T] = CState(s => (x, s))


  class MonadStuff[S] {
    type CState1[T] = CState[S, T]

    implicit def cStateMonad: Monad[CState1] = new Monad[CState1] {
      override def map[A, B](fa: CState[S, A])(f: A => B): CState[S, B] =
        CState(s => {
          val (x, s2) = fa.step(s)
          (f(x), s2)
        })

      override def pure[A](x: A): CState[S, A] =
        CState(s => (x, s))

      override def flatMap[A, B](fa: CState[S, A])(f: A => CState[S, B]): CState[S, B] = {
        CState(s => {
          val (x, s2) = fa.step(s)
          f(x).step(s2)
        })

      }

      override def tailRecM[A, B](a: A)(f: A => CState[S, Either[A, B]]): CState[S, B] = {
        CState { initialState =>
          @scala.annotation.tailrec
          def rec(currentState: S, currentVal: A): (B, S) = {
            val (r, newState) = f(currentVal).step(currentState)
            r match {
              case Left(newVal) =>
                rec(newState, newVal)
              case Right(r) =>
                (r, newState)
            }
          }

          rec(initialState, a)
        }
      }
    }

  }


}