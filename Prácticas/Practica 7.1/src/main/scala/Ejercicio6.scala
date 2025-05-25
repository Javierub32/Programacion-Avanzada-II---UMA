package esqueletosLaboratorio7

import java.util.concurrent.locks.ReentrantLock
import scala.util.Random

class Bandeja(R:Int){

  private var raciones = 0
  private var hayTarta = false

  def quieroRacion(id:Int)= synchronized {
    while (!hayTarta) wait()
    raciones -= 1

    log(s"Niño $id ha cogido una ración. Quedan $raciones")

    if (raciones == 0) {
      hayTarta = false
      notify()
    }
   
  }
  def tarta()= synchronized {
    while (hayTarta) wait()
    log("El pastelero pone una nueva tarta.")
    raciones = R
    hayTarta = true
    notifyAll()
  }
}
/*
class Bandeja(R: Int) {

  private var raciones = 0
  private var hayTarta = false
  private val l = new ReentrantLock(true)
  private val cHayTarta = l.newCondition()
  private val cPoneTarta = l.newCondition()

  def quieroRacion(id: Int) =  {
    l.lock()
    try {
      while (!hayTarta) cHayTarta.await()
      raciones -= 1

      log(s"Niño $id ha cogido una ración. Quedan $raciones")

      if (raciones == 0) {
        hayTarta = false
        cPoneTarta.signal()
      }
    } finally {
      l.unlock()
    }
  }

  def tarta() =  {
    l.lock()
    try {
      while (hayTarta) cPoneTarta.await()
      log("El pastelero pone una nueva tarta.")

      raciones = R
      hayTarta = true
      cHayTarta.signalAll()
    } finally {
      l.unlock()
    }
  }
}
 */

object Ejercicio6 {

  def main(args:Array[String]):Unit = {
    val R = 5
    val N = 10
    val bandeja = new Bandeja(R)
    var niño = new Array[Thread](N)
    for (i<-niño.indices)
      niño(i) = thread{
        while (true){
          Thread.sleep(Random.nextInt(500))
          bandeja.quieroRacion(i)
        }
      }
    val pastelero = thread{
      while (true){
        Thread.sleep(Random.nextInt(100))
        bandeja.tarta()
      }
    }
  }


}
