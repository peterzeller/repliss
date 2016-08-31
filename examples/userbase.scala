
idtype UserId
type String

type userRecordField =
    f_id()
  | f_name()
  | f_mail()

operation mapWrite(uid: UserId, field: userRecordField, value: String)
operation mapDelete(uid: UserId)


invariant forall c1: callId, c2: callId, u: UserId, f: userRecordField, v: String ::
     c1.op == mapDelete(u)
  && c2.op == mapWrite(u, f, v)
  ==> !(c1 happened before c2)

query mapExists(u: UserId): Boolean =
 (exists c1: callId, f: userRecordField, v: String ::
       c1 is visible
    && c1.op == mapWrite(u, f, v)
    && (forall c2: callId :: (c2 is visible && c2.op == mapDelete(u)) ==> c2 happened before c1))


def registerUser(name: String, mail: String): UserId {
  // TODO userId should be generated
  var u: UserId
  u = new UserId
  atomic {
//    call mapWrite(uid, f_name(), uid)
    call mapWrite(u, f_name(), name)
    call mapWrite(u, f_mail(), mail)
  }
  return u
}

def updateMail(id: UserId, newMail: String) {
  var uExists: Boolean
//  atomic {
    uExists = mapExists(id)
    if (uExists) {
      call mapWrite(id, f_mail(), newMail)
    }
//  }
}

def removeUser(id: UserId) {
  call mapDelete(id)
}