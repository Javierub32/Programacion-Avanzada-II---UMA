package concurrencia

object ExclusionMutua {
  def main(args:Array[String]) = {
    var c = 0
    var f0 = false
    var f1 = false

    val h0 = thread {
      for (i <- 0 until 100)
        f0 = true //Indico que quiero entrar a la exclusionMutua
        while (f1) {}

        c += 1 //SC0

        f0 = false
    }
    val h1 = thread {
      for (i <- 0 until 100)
        f1 = true //Indico que quiero entrar a la exclusionMutua
        while (f0) {}

        c += 1 //SC0

        f1 = false
    }

    h0.join()
    h1.join()
    log(s"c = $c")
  }
}
