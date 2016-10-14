idtype UserId

def makeFriends(userA: UserId, userB: UserId) {
  atomic {
    call friendSet_add(userA, userB)
    call friendSet_add(userB, userA)
  }
}

//def unFriend(userA: UserId, userB: UserId) {
//  atomic {
//    call friendSet_remove(userA, userB)
//    call friendSet_remove(userB, userA)
//  }
//}

// each user has a set of friends
operation friendSet_add(key: UserId, value: UserId)
operation friendSet_remove(key: UserId, value: UserId)

//query friendSet_contains(key: UserId, value: UserId): boolean =
//  (exists c1: callId ::
//         c1 is visible
//      && c1.op == friendSet_add(key, value)
//      && (forall c2: callId :: (c2 is visible && c2.op == friendSet_remove(key, value)) ==> c2 happened before c1))

// gset-semantics
// TODO make this inlineable
query friendSet_contains(key: UserId, value: UserId): boolean =
  (exists c1: callId ::
         c1 is visible
      && c1.op == friendSet_add(key, value))

// friendship relation should be symmetric
invariant forall a: UserId, b: UserId ::
  friendSet_contains(a, b) ==> friendSet_contains(b, a)

//
invariant forall a: UserId, b: UserId, c1: callId ::
    c1.op == friendSet_add(a, b)
  ==> (exists c2: callId ::
        c2.op == friendSet_add(b, a)
        && sameTransaction(c1,c2)
    )

// no removes yet (TODO):
//invariant forall c1: callId, a: UserId, b: UserId ::
//  c1.op != friendSet_remove(a, b)
