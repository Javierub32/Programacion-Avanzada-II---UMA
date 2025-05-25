package esqueletosLaboratorio7

import java.util.concurrent.locks.ReentrantLock
import scala.util.Random
/*
object Parejas{
  private var hombreDentro = false
  private var mujerDentro = false

  def llegaHombre(id:Int) = synchronized {
    while (hombreDentro) wait()
    hombreDentro = true
    log(s"Hombre $id quiere formar pareja")

    if (mujerDentro) {
      log(s"Se ha formado una pareja!!")
      hombreDentro = false
      mujerDentro = false
      notifyAll()
    }
  }

  def llegaMujer(id: Int) =  synchronized {
    while (mujerDentro) wait()
    mujerDentro = true
    log(s"Mujer $id quiere formar pareja")

    if (hombreDentro) {
      log(s"Se ha formado una pareja!!")
      hombreDentro = false
      mujerDentro = false
      notifyAll()
    }
  }
}
*/

object Parejas {
  private val l = new ReentrantLock(true)
  private var hombreDentro = false
  private val cHombreDentro = l.newCondition()
  private var mujerDentro = false
  private val cMujerDentro = l.newCondition()

  def llegaHombre(id: Int) =  {
    l.lock()
    try {
      while (hombreDentro) cHombreDentro.await()
      hombreDentro = true
      log(s"Hombre $id quiere formar pareja")

      if (mujerDentro) {
        log(s"Se ha formado una pareja!!")
        mujerDentro = false
        hombreDentro = false
        cMujerDentro.signal()
        cHombreDentro.signal()
      }
    } finally {
      l.unlock()
    }
  }

  def llegaMujer(id: Int) = {
    l.lock()
    try {
      while (mujerDentro) cMujerDentro.await()
      mujerDentro = true
      log(s"Mujer $id quiere formar pareja")

      if (hombreDentro) {
        log(s"Se ha formado una pareja!!")
        mujerDentro = false
        hombreDentro = false
        cMujerDentro.signal()
        cHombreDentro.signal()
      }
    } finally {
      l.unlock()
    }
  }
}
object Ejercicio3 {

  def main(args:Array[String]):Unit = {
    val NP = 10
    val mujer = new Array[Thread](NP)
    val hombre = new Array[Thread](NP)
    for (i<-mujer.indices)
      mujer(i) = thread{
        Parejas.llegaMujer(i)
      }
    for (i <- hombre.indices)
      hombre(i) = thread {
        Parejas.llegaHombre(i)
      }
  }

}
