package esqueletosLaboratorio7

import scala.util.Random

class Buffer(ncons:Int,tam:Int){
  //ncons-número de consumidores
  //tam-tamaño del buffer
  private val buffer = new Array[Int](tam)
  private var bufferCons = Array.fill(tam)(ncons)
  private var idx = 0
  private var index = 0
  private var dato = 0

  def nuevoDato(dato:Int) = synchronized {
    //el productor pone un nuevo dato
    while (buffer.forall(_ != 0)) wait()
    idx = buffer.indexWhere(_ == 0)
    buffer(idx) = dato

    log(s"Productor almacena $dato: buffer=${buffer.mkString("[",",","]")}}")
    notifyAll() // Despierto a el que quisiese consumir
  }

  def extraerDato(id:Int):Int = synchronized {
    while (buffer.forall(_ == 0)) wait() // Si no hay nada que consumir, me paro
    idx  = buffer.indexWhere(_ != 0)    // Busco el primer elemento que pueda consumir
    dato = buffer(idx)

    bufferCons(idx) -= 1

    log(s"Consumidor $id lee $dato: buffer=${buffer.mkString("[",",","]")}")

    if (bufferCons(idx) == 0) {
      bufferCons(idx) = ncons
      buffer(idx) = 0
      notify()
    }
    dato
  }
}
object Ejercicio1 {

  def main(args:Array[String]):Unit = {
    val ncons = 4
    val tam = 3
    val nIter = 10
    val buffer  = new Buffer(ncons,tam)
    val consumidor = new Array[Thread](ncons)
    for (i<-consumidor.indices)
      consumidor(i) = thread{
        for (j<-0 until nIter)
          val dato = buffer.extraerDato(i)
          Thread.sleep(Random.nextInt(200))
      }
    val productor = thread{
      for (i<-0 until nIter)
        Thread.sleep(Random.nextInt(50))
        buffer.nuevoDato(i+1)
    }
  }

}
