package concurrencia

class BufferSimple {
  private var c = 0
  @volatile private var hayDato = false

  def nuevoDato(dato:Int) =
    while(hayDato){} //CS-prod
    c = dato
    hayDato = true //CS-Cons

  def leerDato() =
    while(!hayDato){} //CS-Cons
    val aux = c
    hayDato = false  //CS-Prod
    aux
}

object prodConsSimple {
  def main(args:Array[String]) ={
    val buffer = new BufferSimple

    val prod = thread{
      for(i <- 0 until 50)
        log(s"Productor: $i")
        buffer.nuevoDato(i)
    }

    val cons = thread(
      for(i <- 0 until 50)
        log(s"                Consumidor: ${buffer.leerDato()}")

    )
  }
}
