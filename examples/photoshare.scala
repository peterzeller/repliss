idtype UserId
idtype PhotoId
type Photo

type viewPhotoResult =
      ViewPhoto(photo: Photo)
    | NoPermission()

type photos_get_result =
      PhotoResult(owner: UserId, photo: Photo)
    | NotFound()


// remove-wins set for friends:
operation friendset_add(user: UserId, friend: UserId)
operation friendset_remove(user: UserId, friend: UserId)
query friendset_contains(u: UserId, f: UserId): boolean =
    (exists a: callId ::
           a is visible
        && a.op == friendset_add(u, f)
        && (forall r: callId :: (r is visible && r.op == friendset_remove(u,f)) ==> r happened before a))

// LWW wins map for photos (id -> (owner, photo))
operation photos_store(id: PhotoId, owner: UserId, photo: Photo)

query photos_get(id: PhotoId): photos_get_result
axiom forall id: PhotoId, owner: UserId, photo: Photo ::
    (exists c1: callId ::
           c1 is visible
        && c1.op == photos_store(id, owner, photo)
        && (forall c2: callId, owner2: UserId, photo2: Photo ::
               c2 is visible && c2.op == photos_store(id, owner, photo) ==> c2 happened before c1))
      ==> photos_get(id) == PhotoResult(owner, photo)


axiom forall id: PhotoId, owner: UserId, photo: Photo ::
    (photos_get(id) == PhotoResult(owner, photo))
    ==> (exists c1: callId :: c1 is visible
            && c1.op == photos_store(id, owner, photo))


//invariant forall i: invocationId, u: UserId, f: UserId ::
//    i.info == addFriend(u,f)
//        ==> exists c: callId :: c.origin == i && c.op == friendset_add(u,f)
//
//
//
//invariant forall i: invocationId, u: UserId, f: UserId ::
//    i.info == removeFriend(u,f)
//        ==> exists c: callId :: c.origin == i && c.op == friendset_remove(u,f)
//
//invariant forall i: invocationId, u: UserId, p: Photo, pId: PhotoId ::
//    i.info == sharePhotoWithFriends(u, p, pId)
//        ==> exists c: callId :: c.origin == i && c.op == photos_store(pId, u, p)
//
//invariant forall c: callId, u: UserId, p: Photo, pId: PhotoId ::
//    c.op == photos_store(pId, u, p) && !c.inCurrentInvocation
//        ==> exists i: invocationId :: c.origin == i && i.info == sharePhotoWithFriends(u, p, pId)
//
//invariant forall i: invocationId, u: UserId, p: Photo, pId: PhotoId ::
//    i.info == sharePhotoWithFriends(u, p, pId)
//        ==> forall c: callId :: c.op == photos_store(pId, u, p) ==> c.origin == i
//
//
//invariant forall c1: callId, c2: callId, u1: UserId, p1: Photo, u2: UserId, p2: Photo, pId: PhotoId ::
//       c1.op == photos_store(pId, u1, p1)
//    && c2.op == photos_store(pId, u2, p2)
//        ==> c1 == c2
//
//invariant forall i1: invocationId, i2: invocationId, u1: UserId, p1: Photo, u2: UserId, p2: Photo, pId: PhotoId ::
//       i1.info == sharePhotoWithFriends(u1, p1, pId)
//    && i2.info == sharePhotoWithFriends(u2, p2, pId)
//        ==> i1 == i2
//

invariant forall unfriend: invocationId, share: invocationId, view: invocationId, u: UserId, boss: UserId, partyPhoto: Photo, partyPhotoId: PhotoId, viewResult: viewPhotoResult  ::
     unfriend.info == removeFriend(u, boss)
  && unfriend happened before share
  && share.info == sharePhotoWithFriends(u, partyPhoto, partyPhotoId)
  && (forall addF: invocationId :: addF.info == addFriend(u, boss) ==> !(addF happened after unfriend))
  && view.info == viewPhoto(boss, partyPhotoId, viewResult)
  && share happened before view // TODO remove
  ==> viewResult == NoPermission()




def addFriend(user: UserId, friend: UserId) {
    call friendset_add(user, friend)
}

def removeFriend(user: UserId, friendToRemove: UserId) {
    call friendset_remove(user, friendToRemove)
}

def sharePhotoWithFriends(user: UserId, photo: Photo): PhotoId {
    var id: PhotoId
    id = new PhotoId
    call photos_store(id, user, photo)
}

def viewPhoto(user: UserId, photoId: PhotoId): viewPhotoResult {
    var res:  photos_get_result
    res = photos_get(photoId)
    res match {
        case PhotoResult(owner, photo) =>
            if (friendset_contains(owner, user)) {
                return ViewPhoto(photo)
            } else {
                return NoPermission()
            }
        case NotFound() =>
            return NoPermission()
    }
}

// unused invariants:


// Wrong invariants:

//invariant forall c: callId, u: UserId, f: UserId ::
//    c.op == friendset_add(u,f) && !c.inCurrentInvocation
//        ==> exists i: invocationId :: c.origin == i && i.info == addFriend(u,f)

//invariant forall c: callId, u: UserId, f: UserId ::
//    c.op == friendset_remove(u,f) && !c.inCurrentInvocation
//        ==> exists i: invocationId :: c.origin == i && i.info == removeFriend(u,f)

//invariant forall a: callId, r: callId, u: UserId, f: UserId ::
//       !a.inCurrentInvocation
//    && !r.inCurrentInvocation
//    && a.op == friendset_add(u,f)
//    && r.op == friendset_remove(u,f)
//    ==> ((a happened before r) == (a.origin happened before r.origin))
//
//invariant forall a: callId, r: callId, u: UserId, f: UserId ::
//       !a.inCurrentInvocation
//    && !r.inCurrentInvocation
//    && a.op == friendset_add(u,f)
//    && r.op == friendset_remove(u,f)
//    ==> ((r happened before a) == (r.origin happened before a.origin))


