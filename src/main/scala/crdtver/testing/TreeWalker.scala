package crdtver.testing

import crdtver.utils.LazyListUtils
import crdtver.utils.LazyListUtils.LazyListExtensions

/**
  * enumerate all finite paths through a possibly infinite tree
  */
object TreeWalker {

  case class Tree[+A](elem: A, children: LazyList[Tree[A]]) {
    def print(maxDepth: Int = 5): String ={
      val b = new StringBuilder
      def walk(t: Tree[A], n: Int, level: Int): Unit = {
        if (n >= maxDepth)
          return
        b.append("\n")
        b.append("  " * level)
        b.append(t.elem)

        for ((c, i) <- t.children.take(maxDepth - n).zipWithIndex)
          walk(c, n + 1 + i, level + 1)
      }

      walk(this, 0, 0)
      b.append("\n")
      b.toString
    }
  }






  def tree[A](root: A, children: A => LazyList[A]): Tree[A] = {
    def childTrees(a: A): LazyList[Tree[A]] =
      children(a).map(c => Tree(c, childTrees(c)))
    Tree(root, childTrees(root))
  }


  /** breadth-first search */
  def walkTree[T](root: T, children: T => LazyList[T]): LazyList[List[T]] = {
    def walk(t: T, depth: Int): LazyList[List[T]] = {
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
    def walk(t: T): LazyList[LazyList[T]] = {
//      println(s"walk($t)")
      LazyListUtils.combine(t, new LazyListExtensions(children(t)).breadthFirst.flatMap(c => walk(c)))
    }

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
  def walkTree2[T](t: Tree[T], breadth: Int = 2): LazyList[T] = {

    def walk(level: LazyList[LazyList[Tree[T]]]): LazyList[T] = {
//      level.take(6).map(_.take(3).force).force
//      println(s"walk($level)")
      if (level.isEmpty)
         LazyList()
      else {
        val x = level.head
        val xs = level.tail
        val (x1, x2) = x.splitAt(breadth)
        val elems = x1.map(_.elem)
        elems #::: walk(xs #::: {
          x1.map(_.children) #:::
          (if (x2.isEmpty) LazyList() else LazyList(x2))

        })
      }
    }

    walk(LazyList(LazyList(t)))
  }

  def walkTree3[T](t: Tree[T], breadth: Int = 2): LazyList[T] = {

    sealed trait Cont
    case class WithTree(tree: Tree[T]) extends Cont
    case class WithChildren(children: LazyList[Tree[T]]) extends Cont
    case class Concat(left: Cont, right: Cont) extends Cont
    case class EmptyCont() extends Cont

    def conc(list: LazyList[Cont]): Cont = list match {
      case LazyList() => EmptyCont()
      case LazyList(x) => x
      case x #:: xs => Concat(x, conc(xs))
    }

    def step(cont: Cont): (LazyList[T], Cont) = cont match {
        case WithTree(tree) =>
          // take out root element and continue with children
          (LazyList(tree.elem), WithChildren(tree.children))
        case WithChildren(children) =>
          // process N of the children,
          val (x1, x2) = children.splitAt(breadth)
          if (x1.isEmpty)
            (LazyList(), EmptyCont())
          else
            (LazyList(), Concat(conc(x1.map(WithTree)), WithChildren(x2)))
        case Concat(left, right) =>
          // step into left
          val (xs, cont) = step(left)
          if (xs.isEmpty && cont == EmptyCont())
            step(right)
          else
            (xs, Concat(right, cont))
        case EmptyCont() =>
          (LazyList(), EmptyCont())
      }

    def walk(cont: Cont): LazyList[T] = cont match {
      case EmptyCont() => LazyList()
      case _ =>
        val (list, c) = step(cont)
        list #::: walk(c)
    }


    walk(WithTree(t))
  }

  def walkTree4[T](root: Tree[T], breadth: Int = 2): LazyList[T] = {
    def walk(t: Tree[T], n: Int): LazyList[T] = {
      if (n <= 0) LazyList()
      else
        t.elem #:: t.children.take(n).zipWithIndex.flatMap{ case (c,i) => walk(c, n - i/breadth - 1)}
    }


    LazyList.from(1).flatMap { depth =>
      walk(root, depth)
    }
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
