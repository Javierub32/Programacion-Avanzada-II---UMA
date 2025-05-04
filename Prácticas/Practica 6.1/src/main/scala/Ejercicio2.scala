package Lab61.soluciones

import java.util.concurrent.*
import scala.util.Random

class Cadena(n: Int) {
  // CS-empaquetador-i: espera hasta que hay productos de tipo i
  // CS-colocador: espera si hay n productos en la cadena
  private val tipo = Array.fill(3)(0) // el buffer
  private var cadenaActual = 0 // Productos en cadena
  private var cuentaTotal = 0 // Total paquetes empaquetados
  private val esperaCol = Semaphore(1) // CS- Colocalor

  private var hayPaquete = Array.fill(3)(new Semaphore(0))
  private var mutex = new Semaphore(1)

  def retirarProducto(p: Int) = {
    hayPaquete(p).acquire()                         // Si hay paquete-i, lo empaqueto
    mutex.acquire()
    tipo(p) -= 1                                    // Elimino el paquete-i
    if (tipo(p) > 0) hayPaquete(p).release()        // Si siguen quedando paquetes, sigo permitiendo la retirada
    cuentaTotal += 1                                // Añado uno a la suma total
    if (cadenaActual == n) esperaCol.release()      // Si la cadena estaba llena, la dejo disponible
    cadenaActual -= 1
    log(s"Empaquetador $p retira un producto. Quedan ${tipo.mkString("[",",","]")}")
    mutex.release()
  }
  def nuevoProducto(p:Int) = {
    esperaCol.acquire()                         // Espero a que se puedan meter elementos
    mutex.acquire()
    if (tipo(p) == 0) hayPaquete(p).release()   // Si no había paquetes de este, ahora si hay
    tipo(p) += 1                                // Meto uno al buffer
    cadenaActual += 1
    if (cadenaActual < n) esperaCol.release()   // Si la cadena no está llena, no lo pongo a esperar
    log(s"Colocador pone un producto $p. Quedan ${tipo.mkString("[",",","]")}")
    log(s"Total de productos empaquetados $cuentaTotal")
    mutex.release()
  }
}

object Ejercicio2 {
  def main(args:Array[String]) = {
    val cadena = new Cadena(6)
    val empaquetador = new Array[Thread](3)
    for (i <- 0 until empaquetador.length)
      empaquetador(i) = thread {
        while (true)
          cadena.retirarProducto(i)
          Thread.sleep(Random.nextInt(500)) // empaquetando
      }

    val colocador = thread {
      while (true)
        Thread.sleep(Random.nextInt(100)) // recogiendo el producto
        cadena.nuevoProducto(Random.nextInt(3))
    }
  }
}
