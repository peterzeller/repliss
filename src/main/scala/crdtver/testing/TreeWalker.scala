package crdtver.testing

/**
  * enumerate all finite paths through a possibly infinite tree
  */
object TreeWalker {


  /** breadth-first search */
  def walkTree[T](root: T, children: T => LazyList[T]): LazyList[List[T]] = {
    def walk(t: T, depth: Int): LazyList[List[T]] = {
      println(s"walk($t, $depth)")
      if (depth <= 0) LazyList(List())
      else {
        children(t).flatMap(c => walk(c, depth - 1).map(ct => c :: ct))
      }
    }

    for (depth <- LazyList.iterate(0)(_ + 1); path <- walk(root, depth)) yield path
  }

  /**
    * calculates all paths to leafs through the tree (tree and paths can be infinite)
    */
  def paths[T](root: T, children: T => LazyList[T]): LazyList[LazyList[T]] = {
    def walk(t: T): LazyList[LazyList[T]] =
      for {
        c <- children(t)
        p <- walk(c)
      } yield c #:: p

    walk(root)
  }

  /** iterative bounded breadth and depth search
    *
    * with breadth = 2 it would look something like:
    *
    * ..children -->
    * l 1122334455
    * e 22334455
    * n 334455
    * | 4455
    * v 55
    *
    * */
  def walkTree2[T](root: T, breadth: Int = 2, children: T => LazyList[T]): LazyList[T] = {
    val ps = paths(root, children)

    def walk(n: Int): LazyList[T] = {
      for {
        (p, i) <- ps.take(breadth).zipWithIndex
        s <- p.take(n - i / breadth)
      } yield s
    }

    for (depth <- LazyList.iterate(0)(_ + 1); path <- walk(depth)) yield path
  }

  /*
  def walkTreeA[T, Action](
    root: T,
    applyAction: (T, Action) => Option[T],
    possibleActions: T => LazyList[Action]): LazyList[List[T]] = {
      def walk(t: T, depth: Int): LazyList[List[T]] =
        if (depth <= 0) LazyList(List())
        else {
          for (c <- children(t); ct <- walk(c, depth - 1)) yield c :: ct
        }

      for (depth <- LazyList.iterate(0)(_ + 1); path <- walk(root, depth)) yield path
    }
*/


}
