package crdtver.utils


object EitherExtensions {

  implicit class EitherUtils[L, R](e: Either[L, R]) {

    def takeLeft: Iterable[L] =
      new Iterable[L]() {
        override def iterator: Iterator[L] =
          e.left.toOption.iterator
      }

    def takeRight: Iterable[R] =
      new Iterable[R]() {
        override def iterator: Iterator[R] =
          e.toOption.iterator
      }

  }


}