package object concurrencia {

  def thread(body: => Unit):Thread = {
    val t = new Thread {
      override def run = body
    }
    t.start()  // Pongo la hebra en funcionamiento
    t // Paso la referencia
  }
  
  def log(msg:String) =
    println(s"${Thread.currentThread().getName}: $msg")
}
