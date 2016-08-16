
type UserId
type String

def updateMail(id: UserId, newMail: String) {
  var uExists: Boolean;
  atomic {
    uExists = query_mapExists(id)
    if (uExists) {
      call mapWrite(id, f_mail, newMail)
    }
  }
}