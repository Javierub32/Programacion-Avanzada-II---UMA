package esqueletosLaboratorio7

import java.util.concurrent.locks.ReentrantLock
import scala.util.Random

class Coche(C:Int) extends Thread{
  //CS-pasajero1: si el coche está lleno, un pasajero no puede subir al coche hasta que haya terminado
  //el viaje y se hayan bajado los pasajeros de la vuelta actual
  //CS-pasajero2: un pasajero que está en el coche no se puede bajarse hasta que haya terminado el viaje
  //CS-coche: el coche espera a que se hayan subido C pasajeros para dar una vuelta
  private var numPas = 0
  private var cocheLleno = false
  private var viajeAcabado = false
  private var viajeIniciado = false


  def nuevoPaseo(id:Int)= synchronized {
    while (cocheLleno) wait()
    numPas += 1
    if (numPas == C) {
      cocheLleno = true
      notifyAll()
    }
    log(s"El pasajero $id se sube al coche. Hay $numPas pasajeros.")

    while (!viajeAcabado) wait()
    numPas -= 1
    if (numPas == 0) {
      cocheLleno = false
      viajeAcabado = false
      notifyAll()
    }
    log(s"El pasajero $id se baja del coche. Hay $numPas pasajeros.")
  }

  def esperaLleno = synchronized {
    while (!cocheLleno || viajeAcabado) wait()
    log(s"        Coche lleno!!! empieza el viaje....")
  }

  def finViaje = synchronized {
    println("        Fin del viaje... :-(")
    viajeAcabado    = true
    notifyAll()              // despierta a los pasajeros
    
  }

  override def run = {
    var fin = false
    while (!Thread.interrupted() && !fin){
      try {
        esperaLleno
        Thread.sleep(Random.nextInt(Random.nextInt(500))) //el coche da una vuelta
        finViaje
      } catch {
        case e:InterruptedException => fin = true
      }

    }
  }
}

/*
class Coche2(C: Int) extends Thread {
  //CS-pasajero1: si el coche está lleno, un pasajero no puede subir al coche hasta que haya terminado
  //el viaje y se hayan bajado los pasajeros de la vuelta actual
  //CS-pasajero2: un pasajero que está en el coche no se puede bajarse hasta que haya terminado el viaje
  //CS-coche: el coche espera a que se hayan subido C pasajeros para dar una vuelta
  private var numPas = 0
  private val l = new ReentrantLock(true)
  private var viajeAcabado = false
  private val cViajeAcabado = l.newCondition()
  private var cocheLleno = false
  private val cCocheLleno = l.newCondition()

  def nuevoPaseo(id: Int): Unit = {
    l.lock()
    try {
      // CS-pasajero1: si el coche está lleno, espero
      while (cocheLleno) {
        cCocheLleno.await()
      }

      // Subo al coche
      numPas += 1
      println(s"El pasajero $id se sube al coche. Hay $numPas pasajeros.")

      // Si ya estoy completo, aviso al coche
      if (numPas == C) {
        cocheLleno = true
        cCocheLleno.signal() // despierta al coche en esperaLleno
      }

      // CS-pasajero2: espero a que termine el viaje
      while (!viajeAcabado) {
        cViajeAcabado.await()
      }

      // Me bajo del coche
      numPas -= 1
      println(s"El pasajero $id se baja del coche. Hay $numPas pasajeros.")

      // Si me bajo siendo el último, restauro estado y despierto posibles subidas
      if (numPas == 0) {
        cocheLleno = false
        viajeAcabado = false
        cCocheLleno.signalAll()
      }

    } finally {
      l.unlock()
    }
  }

  /** El coche espera hasta que se llene */
  def esperaLleno(): Unit = {
    l.lock()
    try {
      while (!cocheLleno) {
        cCocheLleno.await()
      }
      println("        Coche lleno!!! empieza el viaje....")
    } finally {
      l.unlock()
    }
  }

  /** Marca el fin de la vuelta y despierta a los pasajeros */
  def finViaje(): Unit = {
    l.lock()
    try {
      println("        Fin del viaje... :-(")
      viajeAcabado = true
      cViajeAcabado.signalAll()
    } finally {
      l.unlock()
    }
  }

  override def run = {
    while (true) {
      esperaLleno()
      Thread.sleep(Random.nextInt(Random.nextInt(500))) //el coche da una vuelta
      finViaje()
    }
  }
}
*/

object Ejercicio4 {
  def main(args:Array[String])=
    val coche = new Coche(5)
    val pasajero = new Array[Thread](20)
    coche.start()
    for (i<-0 until pasajero.length)
      pasajero(i) = thread{
   //     while (true)
          Thread.sleep(Random.nextInt(500))
          coche.nuevoPaseo(i)
      }


    pasajero.foreach(_.join())
    coche.interrupt()
    coche.join()
    log("Fin del programa")
}
