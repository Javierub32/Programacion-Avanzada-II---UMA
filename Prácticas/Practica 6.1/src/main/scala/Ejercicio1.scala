package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

object mediciones {
  // CS-Sensor-i: sensor i no puede volver a medir hasta que el trabajador no ha
  // terminado de procesar las medidas anteriores
  // CS-Trabajador: no puede realizar su tarea hasta que no están las
  // tres mediciones
  private val N = 3
  private var numSensores = 3
  private val mutex = new Semaphore(1)
  private val medicionDisponible = new Semaphore(0)
  private val tareaAcabada = new Semaphore(0)
  private val haySensor = Array.fill(N)(new Semaphore (1))

  def nuevaMedicion(id: Int) = {
    haySensor(id).acquire() // Si ya ha hecho la medición, se queda pillado
    mutex.acquire()         // Hago que no puedan tocar la variable 2 a la vez
    log(s"Sensor $id almacena su medición" )
    numSensores -= 1
    if (numSensores == 0) medicionDisponible.release()
    mutex.release()
  }

  def leerMediciones() = {
    medicionDisponible.acquire()
    mutex.acquire()
    log(s"El trabajador recoge las mediciones")
    tareaAcabada.release()
    mutex.release()
  }

  def finTarea() = {
    tareaAcabada.acquire()
    mutex.acquire()
    log(s"El trabajador ha terminado sus tareas")
    numSensores = N
    for (i <- 0 until N) haySensor(i).release()
    mutex.release()
  }
}

object Ejercicio1 {
  def main(args: Array[String]) =
    val sensor = new Array[Thread](3)

    for (i <- 0 until sensor.length)
      sensor(i) = thread {
        while (true)
          Thread.sleep(Random.nextInt(1000)) // midiendo
          mediciones.nuevaMedicion(i)
      }

    val trabajador = thread {
      while (true)
        mediciones.leerMediciones()
        Thread.sleep(Random.nextInt(1000)) // realizando la tarea
        mediciones.finTarea()
    }
}
