package PAII_IS_A1

// Javier Urbaneja Benítez 79286628Q

import java.util.concurrent.Semaphore
import scala.util.Random
class Salon2(cap:Int){
  private var numEstudiantes = 0
  private val puedoEntrar = new Semaphore(1)
  private val despiertoDecano = new Semaphore(0)
  private val decanoLlega = new Semaphore(0)
  private val salaVacia = new Semaphore(1)
  private val puedoSalir = new Semaphore(1)
  private val mutex = new Semaphore(1)


  /*
   * Condiciones sincronización del ejercicio 1
   * CS-Dec1: El decano no entra en la salon hasta que lo avisan de que se ha excedido el aforo
   * CS-Dec2: El decano espera a que se vacíe el salón para volver a dormir
   * CS-Est1: Un estudiante no puede entrar si el decano está en ella
   */

     /*
     * Condiciones sincronización del ejercicio 2
     * CS-Est2: Un estudiante que está en la fiesta no puede salir si el decano ha sido avisado
     */

  def llegoAFiesta(id:Int)={
    puedoEntrar.acquire()
    mutex.acquire()
    if (numEstudiantes == 0) salaVacia.acquire()
    numEstudiantes += 1
    log(s"Estudiante $id llega a la fiesta. Hay $numEstudiantes")

    if (numEstudiantes == cap) {
      log(s"Estudiante $id avisó al decano. Hay $numEstudiantes")
      despiertoDecano.release()
      puedoSalir.acquire()
    }
    puedoEntrar.release()
    mutex.release()
  }

  def salgoFiesta(id:Int)={
    puedoSalir.acquire()
    mutex.acquire()
    numEstudiantes -= 1
    log(s"Estudiante $id sale de la fiesta. Hay $numEstudiantes")
    if (numEstudiantes == 0) salaVacia.release()
    puedoSalir.release()
    mutex.release()
  }

  def meDuermo()= {
    despiertoDecano.acquire()
    log(s"Decano: me despierto")
    log(s"Decano: entro en la sala: ya sé quienes son. Los estudiantes pueden salir")
    puedoSalir.release()
    decanoLlega.release()
  }
  def esperoTodosFuera()={
    decanoLlega.acquire()
    puedoEntrar.acquire()
    log(s"Decano: espero que salgan todos")
    salaVacia.acquire()
    log(s"Decano: me voy otra vez a dormir")
    salaVacia.release()
    puedoEntrar.release()

  }
}
object ejemResidencia2 {

  def main(args:Array[String]):Unit={
    val R = 20
    val Cap = 5
    val F = 1
    val salon = new Salon2(Cap)
    val estudiante = new Array[Thread](R)
    for (i<-estudiante.indices)
      estudiante(i) = thread{
        for (j<-0 until F)
          Thread.sleep(Random.nextInt(700))
          salon.llegoAFiesta(i)
          Thread.sleep(100)
          salon.salgoFiesta(i)
          Thread.sleep(700)
      }
    val decano = thread{
      var fin =false
      while (!fin && !Thread.interrupted()){
        try{
          Thread.sleep(Random.nextInt(200))
          salon.meDuermo()
          salon.esperoTodosFuera()
        } catch {
          case e:InterruptedException => fin = true
        }
      }
    }
    estudiante.foreach(_.join())
    decano.interrupt()
    decano.join()
    log(s"Todos los estudiantes y el decano se han ido a dormir")
  }

}
