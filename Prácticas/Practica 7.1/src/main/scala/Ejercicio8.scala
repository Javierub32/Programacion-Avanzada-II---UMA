package esqueletosLaboratorio7

import scala.util.Random

object gestorAgua {
  //CS-Hid1: El hidrógeno que quiere formar una molécula espera si ya hay dos hidrógenos
  //CS-Hid2: Un hidrógeno debe esperar a los otros dos átomos para formar la molécula
  //CS-Ox1: El oxígeno que quiere formar una molécula espera si ya hay un oxígeno
  //CS-Ox2: El oxígeno debe esperar a los otros dos átomos para formar la molécula
  private var nHidrogeno = 0
  private var suficienteH = false
  private var suficienteO = false

  def hidrogeno(id: Int) = synchronized {
    while (suficienteH) wait()
    nHidrogeno += 1
    if (nHidrogeno == 2) suficienteH = true

    log(s"Hidrógeno $id quiere formar una molécula")

    if (suficienteH && suficienteO) {
      log(s"      Molécula formada!!!")
      suficienteO = false
      suficienteH = false
      nHidrogeno = 0
      notifyAll() // Despierto a los H y O que estés durmiendo
    }
  }



  def oxigeno(id: Int) = synchronized {
    while (suficienteO) wait()
    suficienteO = true

    log(s"Oxígeno $id quiere formar una molécula")

    if (suficienteH && suficienteO) {
      log(s"      Molécula formada!!!")
      suficienteO = false
      suficienteH = false
      nHidrogeno = 0
      notifyAll() // Despierto a los H y O que estés durmiendo
    }
  }

}
object Ejercicio8 {

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
