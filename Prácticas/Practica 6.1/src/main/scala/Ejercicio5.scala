package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

object gestorAgua {
  // CS-Hid1: El hidrógeno que quiere formar una molécula espera si ya hay dos hidrógenos
  // CS-Hid2: Un hidrógeno debe esperar a los otros dos átomos para formar la molécula
  // CS-Ox1: El oxígeno que quiere formar una molécula espera si ya hay un oxígeno
  // CS-Ox2: El oxígeno debe esperar a los otros dos átomos para formar la molécula
  private var numHidrogenos = 0
  private var numOxigenos = 0
  private val hayOxigeno = new Semaphore(1)
  private val hayHidrogeno = new Semaphore(1)
  private val mutex = new Semaphore(1)

  def oxigeno(id: Int) = {
    hayOxigeno.acquire()                              // Hasta que termine de generarse, no se generan más
    mutex.acquire()
    numOxigenos = 1
    log(s"Oxígeno $id quiere formar una molécula")
    if (numHidrogenos == 2) {                         // Si ya hay suficientes hidrogenos, creo la molécula
      hayOxigeno.release()                            // Restauro todo 
      hayHidrogeno.release()
      numHidrogenos = 0
      numOxigenos = 0
      log(s"      Molécula formada!!!")
    }
    mutex.release()
  }

  def hidrogeno(id: Int) = {
    hayHidrogeno.acquire()
    mutex.acquire()
    numHidrogenos += 1
    if (numHidrogenos == 1) hayHidrogeno.release()
    log(s"Hidrógeno $id quiere formar una molécula")
    if (numHidrogenos == 2 && numOxigenos == 1) {     // Si puedo crear la molécula, la creo
      hayOxigeno.release()                            // Restauro todo 
      hayHidrogeno.release()
      numHidrogenos = 0
      numOxigenos = 0
      log(s"      Molécula formada!!!")
    }

    mutex.release()
  }
}
object Ejercicio5 {

  def main(args:Array[String]) =
    val N = 5
    val hidrogeno = new Array[Thread](2*N)
    for (i<-0 until hidrogeno.length)
      hidrogeno(i) = thread{
        Thread.sleep(Random.nextInt(500))
        gestorAgua.hidrogeno(i)
      }
    val oxigeno = new Array[Thread](N)
    for(i <- 0 until oxigeno.length)
      oxigeno(i) = thread {
        Thread.sleep(Random.nextInt(500))
        gestorAgua.oxigeno(i)
      }
    hidrogeno.foreach(_.join())
    oxigeno.foreach(_.join())
    log("Fin del programa")
}
