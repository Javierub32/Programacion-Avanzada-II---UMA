package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

object mesa {
  // CS-fumador i: No puede fumar hasta que estén en la mesa los ingredientes que le faltan
  // CS-Agente: No pone un nuevo ingrediente hasta que el fumador no ha terminado de fumar

  private val mesaVacia = new Semaphore(1)
  // un semáforo por fumador i (0 = tabaco, 1 = papel, 2 = cerillas)
  private val puedeFumar = Array.fill(3)(new Semaphore(0))

  def quieroFumar(i: Int): Unit = {
    puedeFumar(i).acquire()
    println(s"Fumador $i empieza a fumar")
  }

  def finFumar(i: Int): Unit = {
    println(s"Fumador $i termina de fumar")
    mesaVacia.release()
  }

  def nuevosIngr(ingr: Int): Unit = {
    mesaVacia.acquire()
    println(s"El agente no pone ingrediente $ingr")
    puedeFumar(ingr).release()
  }

}

object Ejercicio6 {
  def main(args: Array[String]): Unit =
    val fumador = new Array[Thread](3)
    for (i <- fumador.indices)
      fumador(i) = thread {
        while (true)
          Thread.sleep(Random.nextInt(500))
          mesa.quieroFumar(i)
          Thread.sleep(Random.nextInt(200))
          mesa.finFumar(i)
      }
    val agente = thread {
      while (true)
        Thread.sleep(Random.nextInt(500))
        mesa.nuevosIngr(Random.nextInt(3))
    }
}
