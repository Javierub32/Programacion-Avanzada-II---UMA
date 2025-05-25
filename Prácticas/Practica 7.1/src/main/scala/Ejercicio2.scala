package esqueletosLaboratorio7

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable.ListBuffer
import scala.util.Random
/*
class Recursos(rec:Int) {
  private var numRec = rec
  private val enEspera = new ListBuffer[Int]()
  private var sigProceso = -1
  private var procesosEsperando = 0


  def pidoRecursos(id:Int,num:Int) = synchronized {
    // Pido recursos
    log(s"Proceso $id pide $num recursos.")
    procesosEsperando += 1
    if (procesosEsperando >= 2) {
      enEspera.addOne(id)
      while (sigProceso != id) wait() // CS-1   Solo salgo cuando me toque
    }

    // Consumo recursos
    while (num > numRec) wait()     // CS-2   Solo cojo cuando haya qué coger
    numRec -= num
    log(s"Proceso $id coge $num recursos. Quedan $numRec")
    procesosEsperando -= 1
    if (procesosEsperando > 0) {
      sigProceso = enEspera.remove(0)
      notifyAll()
    } else {
      sigProceso = -1
    }
  }

  def libRecursos(id:Int,num:Int) = synchronized {
    // Devuelvo los recursos
    numRec += num
    log(s"Proceso $id devuelve $num recursos. Quedan $numRec")
    notifyAll()
  }
}
*/


class Recursos(rec: Int) {
  private var numRec = rec
  private var procesosEsperando = 0
  private var turno = -1
  private val enEspera = new ListBuffer[Int]()

  // Lock justo y dos condiciones:
  private val l = new ReentrantLock(true)
  private val turnoCond = l.newCondition()
  private val recursoCond = l.newCondition()

  def pidoRecursos(id: Int, num: Int): Unit = {
    l.lock()
    try {
      println(s"[pido] Proceso $id pide $num recursos.")
      procesosEsperando += 1

      // Encolar y si nadie tiene turno, dárselo a este
      if (turno == -1) {
        turno = id
      } else {
        enEspera += id
      }

      // Espero a que sea mi turno
      while (turno != id) {
        turnoCond.await()
      }

      // Espero a que haya suficientes recursos
      while (num > numRec) {
        recursoCond.await()
      }

      // Ya puedo consumir
      numRec -= num
      println(s"[coge] Proceso $id coge $num recursos. Quedan $numRec.")
      procesosEsperando -= 1

      // Preparo el siguiente turno FIFO
      if (enEspera.nonEmpty) {
        turno = enEspera.remove(0)
      } else {
        turno = -1
      }

      // Señalo a todos: el que tenga el turno podrá entrar, o quien espere recursos
      turnoCond.signalAll()
      recursoCond.signalAll()

    } finally {
      l.unlock()
    }
  }

  def libRecursos(id: Int, num: Int): Unit = {
    l.lock()
    try {
      // Devuelvo recursos
      numRec += num
      println(s"[libera] Proceso $id devuelve $num recursos. Quedan $numRec.")
      // Aviso a cualquiera que espere turno o recursos
      turnoCond.signalAll()
      recursoCond.signalAll()
    } finally {
      l.unlock()
    }
  }
}

object Ejercicio2 {

  def main(args:Array[String]):Unit = {
    val rec = 5
    val numProc = 10
    val recursos = new Recursos(rec)
    val proceso = new Array[Thread](numProc)
    for (i<-proceso.indices)
      proceso(i) = thread{
      //  while (true){
          val r = Random.nextInt(rec)+1
          recursos.pidoRecursos(i,r)
          Thread.sleep(Random.nextInt(300))
          recursos.libRecursos(i,r)
     //   }
      }
  }
}
