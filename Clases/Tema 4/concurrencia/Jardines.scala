package concurrencia

object Jardines {
  def main(args:Array[String]) = {
    var c = 0
    val p1 = thread(
      for(i <- 0 until 100)
        c+=1
    )
    val p2 = thread(
      for(i <- 0 until 100)
        c+=1
    )

    p1.join()
    p2.join()
    log(s"c = $c")
  }
}
