package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random
object aseo{
  // CS-Cliente: Esperan si está el Equipo de Limpieza en el aseo
  // CS-EquipoLimpieza: Espera si hay clientes en el aseo

  private var numClientes = 0
  private val mutex = new Semaphore(1)
  private val aseoLibre = new Semaphore(1)
  private val sePuedeEntrar = new Semaphore(1)

  def entraCliente(id:Int)={
    sePuedeEntrar.acquire()                               // Si no hay nadie limpiando, entro
    mutex.acquire()
    if (numClientes == 0) aseoLibre.acquire()             //Si entra el primer cliente, pongo que el aseo no está libre
    numClientes += 1
    log(s"Entra cliente $id. Hay $numClientes clientes.")
    sePuedeEntrar.release()                               // Mientras que haya gente se puede seguir entrando
    mutex.release()
  }
  def saleCliente(id:Int)={
    mutex.acquire()
    numClientes -= 1
    if (numClientes == 0) aseoLibre.release()           // Si ya no hay clientes, el aseo se queda libre
    log(s"Sale cliente $id. Hay $numClientes clientes.")
    mutex.release()
  }
  def entraEquipoLimpieza ={
    aseoLibre.acquire()                             // Si pillan el aseo libre, limpian
    mutex.acquire()
    sePuedeEntrar.acquire()                         // Se hace que no se pueda entrar mientras limpian
    log(s"        Entra el equipo de limpieza.")
    mutex.release()
  }
  def saleEquipoLimpieza = {
    mutex.acquire()
    log(s"        Sale el equipo de limpieza.")
    sePuedeEntrar.release()                       // Se permite la entrada
    aseoLibre.release()                           // Se vuelve a poner libre el aseo      
    mutex.release()
  }
}

object Ejercicio3 {
  def main(args: Array[String]) = {
    val cliente = new Array[Thread](10)
    for (i <- 0 until cliente.length)
      cliente(i) = thread {
        while (true)
          Thread.sleep(Random.nextInt(500))
          aseo.entraCliente(i)
          Thread.sleep(Random.nextInt(50))
          aseo.saleCliente(i)
      }
    val equipoLimpieza = thread {
      while (true)
        Thread.sleep(Random.nextInt(500))
        aseo.entraEquipoLimpieza
        Thread.sleep(Random.nextInt(100))
        aseo.saleEquipoLimpieza
    }
  }
}
