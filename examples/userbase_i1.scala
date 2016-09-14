// application specification:

invariant (forall r: invocationId, g: invocationId, u: UserId, res: getUserResult  ::
     r.info == removeUser(u)
  && g.info == getUser(u, res)
  && r happened before g
  ==> res == notFound())

// application implementation:

def registerUser(name: String, mail: String): UserId {
  var u: UserId
  u = new UserId
  atomic {
    call mapWrite(u, f_name(), name)
    call mapWrite(u, f_mail(), mail)
  }
  return u
}

def updateMail(id: UserId, newMail: String) {
  var uExists: boolean
  atomic {
    uExists = mapExists(id)
    if (uExists) {
      call mapWrite(id, f_mail(), newMail)
    }
  }
}

def removeUser(id: UserId) {
  call mapDelete(id)
}

def getUser(id: UserId): getUserResult {
  atomic {
    if (mapExists(id)) {
      return found(mapGet(id, f_name()), mapGet(id, f_mail()))
    } else {
      return notFound()
    }
  }
}

// used types:

idtype UserId
type String

type userRecordField =
    f_id()
  | f_name()
  | f_mail()

type getUserResult =
    notFound()
  | found(name: String, mail: String)

// CRDT specifications
operation mapWrite(uid: UserId, field: userRecordField, value: String)
operation mapDelete(uid: UserId)

query mapExists(u: UserId): boolean =
(exists c1: callId, f: userRecordField, v: String ::
       c1 is visible
    && c1.op == mapWrite(u, f, v)
    && (forall c2: callId :: (c2 is visible && c2.op == mapDelete(u)) ==> c2 happened before c1))

query mapGet(u: UserId, f: userRecordField): String

