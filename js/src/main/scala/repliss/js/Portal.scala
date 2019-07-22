package repliss.js

class Portal[T] {
  private var readHandler: Option[() => T] = None
  def read: T = readHandler.getOrElse(throw new RuntimeException("Could not read value")).apply()

  def setHandler(h: () => T): Unit = {
    this.readHandler = Some(h)
  }
}
