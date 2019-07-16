package crdtver.testing

/**
  * enumerate all finite paths through a possibly infinite tree
  */
object TreeWalker {


  def walkTree[T](root: T)(children: T => Stream[T]): Stream[List[T]] = {
    def walk(t: T, depth: Int): Stream[List[T]] =
      if (depth <= 0) Stream(List())
      else {
        for (c <- children(t); ct <- walk(c, depth - 1)) yield c :: ct
      }

    for (depth <- Stream.iterate(0)(_ + 1); path <- walk(root, depth)) yield path
  }

  /*
  def walkTreeA[T, Action](
    root: T,
    applyAction: (T, Action) => Option[T],
    possibleActions: T => Stream[Action]): Stream[List[T]] = {
      def walk(t: T, depth: Int): Stream[List[T]] =
        if (depth <= 0) Stream(List())
        else {
          for (c <- children(t); ct <- walk(c, depth - 1)) yield c :: ct
        }

      for (depth <- Stream.iterate(0)(_ + 1); path <- walk(root, depth)) yield path
    }
*/


}
