package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

class Nido(B: Int) {
  // CS-bebé i: no puede coger un bichito del plato si está vacío
  // CS-papá/mamá: no puede dejar un bichito en el plato si está lleno

  private var plato = 0
  private val mutex = new Semaphore(1)
  private val lleno = new Semaphore(1)
  private val hayBichos = new Semaphore(0)
  // ...

  def cojoBichito(i: Int) = {
    hayBichos.acquire()
    mutex.acquire()
    if (plato == B) lleno.release()
    plato -= 1
    if (plato > 0) hayBichos.release()
    log(s"Bebé $i coge un bichito. Quedan $plato bichitos")
    mutex.release()
  }

  def pongoBichito(i: Int) = {
    lleno.acquire()
    mutex.acquire()
    if (plato == 0) hayBichos.release()
    plato += 1
    if (plato != B) lleno.release()
    log(s"Papá $i pone un bichito. Quedan $plato bichitos")
    mutex.release()
  }
}

object Ejercicio7 {
  def main(args: Array[String]): Unit =
    val N = 10
    val nido = new Nido(5)
    val bebe = new Array[Thread](N)
    for (i <- bebe.indices)
      bebe(i) = thread {
        while (true)
          nido.cojoBichito(i)
          Thread.sleep(Random.nextInt(600))
      }
    val papa = new Array[Thread](2)
    for (i <- papa.indices)
      papa(i) = thread {
        while (true)
          Thread.sleep(Random.nextInt(100))
          nido.pongoBichito(i)
      }
}
