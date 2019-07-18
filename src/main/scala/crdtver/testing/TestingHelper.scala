package crdtver.testing

import crdtver.testing.Interpreter.{InvocationId, LocalState, State, TransactionId}

import scala.collection.immutable.{::, Nil}

object TestingHelper {

  def getPulledTransactions2(state: State, invoc: InvocationId): LazyList[Set[TransactionId]] = {
    val allTransactions = state.transactions.keySet
    val ls: LocalState = state.localStates.getOrElse(invoc, return LazyList(Set()))

    // get transactions, which are not yet in current snapshot
    val pullableTransactions = allTransactions.filter(tx => !ls.isTransactionVisible(tx, state))

    getValidSnapshots(state, pullableTransactions)
  }

  def getValidSnapshots(state: State, transactions: Set[TransactionId]): LazyList[Set[TransactionId]] = {
    println(s"getValidSnapshots $transactions")
    for (t1 <- transactions) {
      println(s"  $t1 calls: ${state.transactions(t1).currentCalls}")
      for (t2 <- transactions)
        if (happensBefore(t1, t2))
          println(s"  $t1 happensBefore $t2")
        else
          println(s"  $t1 not  before $t2")
    }
    def happensBefore(t1: TransactionId, t2: TransactionId): Boolean = {
      val a = state.transactions(t1)
      val b = state.transactions(t2)
      a.happenedBefore(b)
    }

    allDownwardsClosedSubsets(transactions, happensBefore)
  }

  private def allDownwardsClosedSubsets[T](set: Set[T], lessThan: (T, T) => Boolean)(implicit ordering: Ordering[T]): LazyList[Set[T]] = {
    if (set.isEmpty)
      return LazyList(Set())
    // take minimal elements
    val m = minimalElements(set, lessThan)
    val remaining1 = set -- m
    for {
      // all subsets of minimal elements
      ms <- allSubsets(m)
      // let remaining2 be the non-minimal elements that still have all dependencies from m in ms
      other = m -- ms
      remaining2 = remaining1.filter(x => !other.exists(y => lessThan(y, x)))
      // recursively get all combinations for the rest
      rs <- allDownwardsClosedSubsets(remaining2, lessThan)
    } yield ms ++ rs
  }

  private def minimalElements[T](set: Set[T], lessThan: (T, T) => Boolean): Set[T] = {
    set.filter(x => !set.exists(y => lessThan(x, y)))
  }

  private def allSubsets[T](set: Set[T]): LazyList[Set[T]] = {
    allSublists(set.toList).map(_.toSet)
  }

  private def allSublists[T](list: List[T]): LazyList[List[T]] = list match {
    case Nil =>
      LazyList(List())
    case x :: xs =>
      for (sl <- allSublists(xs); y <- LazyList(sl, x :: sl)) yield y
  }

}
