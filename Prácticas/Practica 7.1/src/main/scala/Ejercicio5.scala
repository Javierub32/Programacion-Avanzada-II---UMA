package esqueletosLaboratorio7

import java.util.concurrent.locks.ReentrantLock
import scala.util.Random

object Barca{
  private var nIPhone = 0
  private var nAndroid = 0
  private var todosBajados = true
  private var viajeAcabado = false
  

  def paseoIphone(id:Int) = synchronized {
    while (nIPhone >= 2 || !todosBajados) wait()
    nIPhone += 1

    log(s"Estudiante IPhone $id se sube a la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

    if (nAndroid == 2 && nIPhone == 2) {
      todosBajados = false

      log(s"Empieza el viaje....")
      Thread.sleep(Random.nextInt(200))
      log(s"fin del viaje....")

      viajeAcabado = true
      notifyAll()     // Despierto a las personas que estuvieran esperando en el wait de viajeAcabado
    }

    while (!viajeAcabado) wait()
    nIPhone -= 1

    log(s"Estudiante IPhone $id se baja de la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

    if (nIPhone == 0 && nAndroid == 0) {
      viajeAcabado = false
      todosBajados = true
      notifyAll() // Despierto a todos los que quieran subirse
    }
   
  }

  def paseoAndroid(id:Int) = synchronized {

    while (nAndroid >= 2 || !todosBajados) wait()
    nAndroid += 1

    log(s"Estudiante Android $id se sube a la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

    if (nAndroid == 2 && nIPhone == 2) {
      todosBajados = false

      log(s"Empieza el viaje....")
      Thread.sleep(Random.nextInt(200))
      log(s"fin del viaje....")

      viajeAcabado = true
      notifyAll()     // Despierto a las personas que estuvieran esperando en el wait de viajeAcabado
    }

    while (!viajeAcabado) wait()
    nAndroid -= 1

    log(s"Estudiante Android $id se baja de la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

    if (nIPhone == 0 && nAndroid == 0) {
      viajeAcabado = false
      todosBajados = true
      notifyAll() // Despierto a todos los que quieran subirse
    }
  }
}
/*
object Barca {
  private var nIPhone = 0
  private var nAndroid = 0
  private val l = new ReentrantLock(true)
  private val cPuedoEntrar = l.newCondition()
  private val cViajeAcabado = l.newCondition()
  private var todosBajaron = true
  private var viajeAcabado = false


  def paseoIphone(id: Int) =  {
    l.lock()
    try {
      while (nIPhone >= 2 || !todosBajaron) cPuedoEntrar.await()
      nIPhone += 1

      log(s"Estudiante IPhone $id se sube a la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

      if (nIPhone == 2 && nAndroid == 2) {
        todosBajaron = false

        log(s"Empieza el viaje....")
        Thread.sleep(Random.nextInt(200))
        log(s"fin del viaje....")

        viajeAcabado = true
        cViajeAcabado.signalAll()
      }

      while (!viajeAcabado) cViajeAcabado.await()
      nIPhone -= 1

      log(s"Estudiante IPhone $id se baja de la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

      if (nIPhone == 0 && nAndroid == 0) {
        viajeAcabado = false
        todosBajaron = true
        cPuedoEntrar.signalAll()
      }
    } finally {
      l.unlock()
    }
  }

  def paseoAndroid(id: Int) =  {
    l.lock()
    try {
      while (nAndroid >= 2 || !todosBajaron) cPuedoEntrar.await()
      nAndroid += 1

      log(s"Estudiante Android $id se sube a la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

      if (nIPhone == 2 && nAndroid == 2) {
        todosBajaron = false

        log(s"Empieza el viaje....")
        Thread.sleep(Random.nextInt(200))
        log(s"fin del viaje....")

        viajeAcabado = true
        cViajeAcabado.signalAll()
      }

      while (!viajeAcabado) cViajeAcabado.await()
      nAndroid -= 1

      log(s"Estudiante Android $id se baja de la barca. Hay: iphone=$nIPhone,android=$nAndroid ")

      if (nIPhone == 0 && nAndroid == 0) {
        viajeAcabado = false
        todosBajaron = true
        cPuedoEntrar.signalAll()
      }
    } finally {
      l.unlock()
    }
  }
}
*/

object Ejercicio5 {

  def main(args:Array[String]) = {
    val NPhones = 10
    val NAndroid = 10
    val iphone = new Array[Thread](NPhones)
    val android = new Array[Thread](NAndroid)
    for (i<-iphone.indices)
      iphone(i) = thread{
     //   while (true){
          Thread.sleep(Random.nextInt(400))
          Barca.paseoIphone(i)
        //    }
      }
    for (i <- android.indices)
      android(i) = thread {
     //   while (true) {
          Thread.sleep(Random.nextInt(400))
          Barca.paseoAndroid(i)
     //   }
      }
  }
}
