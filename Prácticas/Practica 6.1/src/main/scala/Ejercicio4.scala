package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

class Coche(C: Int) extends Thread {
  // CS-pasajero1: si el coche está lleno, un pasajero no puede subir al coche hasta que haya terminado
  // el viaje y se hayan bajado los pasajeros de la vuelta actual
  // CS-pasajero2: un pasajero que está en el coche no puede bajarse hasta que haya terminado el viaje
  // CS-coche: el coche espera a que se hayan subido C pasajeros para dar una vuelta
  private var numPas = 0
  private val mutex = new Semaphore(1)
  private val hayEspacio = new Semaphore(1)
  private val viajeAcabado = new Semaphore(0)
  private val cocheLleno = new Semaphore(0)

  def nuevoPaseo(id: Int) = {
    hayEspacio.acquire()
    mutex.acquire()
    numPas += 1
    if (numPas == C) cocheLleno.release()
    if (numPas < C) hayEspacio.release()
    log(s"El pasajero $id se sube al coche. Hay $numPas pasajeros.")
    mutex.release()

    viajeAcabado.acquire()
    mutex.acquire()
    numPas -= 1
    if (numPas > 0) viajeAcabado.release()          // Si siguen quedando pasajeros, vuelvo a decirle al semáforo que el viaje se acabó
    if (numPas == 0) hayEspacio.release()
    log(s"El pasajero $id se baja del coche. Hay $numPas pasajeros.")
    mutex.release()
  }

  def esperaLleno = {
    cocheLleno.acquire()
    mutex.acquire()
    log(s"        Coche lleno!!! empieza el viaje....")
  }

  def finViaje = {
    // el coche indica que se ha terminado el viaje
    // ...
    log(s"        Fin del viaje... :-(")
    viajeAcabado.release()
    mutex.release()
  }

  override def run = {
    while (true) {
      esperaLleno
      Thread.sleep(Random.nextInt(Random.nextInt(500))) // el coche da una vuelta
      finViaje
    }
  }
}

object Ejercicio4 {
  def main(args: Array[String]) =
    val coche = new Coche(5)
    val pasajero = new Array[Thread](12)
    coche.start()
    for (i <- 0 until pasajero.length)
      pasajero(i) = thread {
        while (true)
          Thread.sleep(Random.nextInt(500)) // el pasajero se da una vuelta por el parque
          coche.nuevoPaseo(i)
      }
}
