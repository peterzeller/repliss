package repliss.js


trait Signal[T] {
  def foreach(f: T => Unit): Unit

  def map[U](f: T => U): Signal[U] = {
    val r = new Signal.Producer[U]()
    for (v <- this) {
      r.send(f(v))
    }
    r
  }

}


object Signal {

  class Producer[T]() extends Signal[T] {
    private var closed = false;
    private var value: Option[T] = None
    private var observers: List[T => Unit] = List()

    override def foreach(f: T => Unit): Unit = {
      require(!closed)
      observers = f :: observers
      for (v <- value) f(v)
    }

    def send(t: T): Unit = {
      require(!closed)
      value = Some(t)
      for (o <- observers)
        o(t)
    }

    def close(): Unit = {
      observers = List()
      value = None
      closed = true
    }

  }
}