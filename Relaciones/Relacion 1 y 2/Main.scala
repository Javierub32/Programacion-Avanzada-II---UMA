object Relacion12 {
  // 1. Implementa una función que compruebe si una cadena dada es un palíndromo (ignorando mayúsculas y minúsculas y espacios). Nota: utilizar las funciones de la librería para la clase String.
  def palindromo(palabra: String): Boolean = {
    val limpia = palabra.toLowerCase.replaceAll("\\s", "")
    limpia == limpia.reverse
  }

  // 2. Crea un programa que imprima los primeros n números primos.
  def esPrimo(num: Int): Boolean = {
    if (num < 2) false
    else !(2 to Math.sqrt(num).toInt).exists(num % _ == 0)
  }

  def primerosPrimos(n: Int): Unit = {
    var count = 0
    var num = 2
    while (count < n) {
      if (esPrimo(num)) {
        print(s"$num ")
        count += 1
      }
      num += 1
    }
    println()
  }

  // 3. Implementa un programa que calcule el máximo común divisor (MCD) y el mínimo común múltiplo (MCM) de dos números. Nota: el tipo devuelto por la función es una tupla de dos componentes.
  def maximoMinimo(n1: Int, n2: Int): (Int, Int) = {
    // Función para calcular el MCD usando el algoritmo de Euclides
    def mcd(a: Int, b: Int): Int = if (b == 0) a else mcd(b, a % b)

    val maximo = mcd(n1, n2)
    val minimo = (n1 * n2) / maximo // Fórmula MCM: (a * b) / MCD(a, b)

    (maximo, minimo)
  }


  // 4. Implementa una función que encuentre el segundo elemento más grande de una lista. Nota: Utilizar las funciones de la librería de la clase List.
  def segundoElemento(l: List[Int]): Int = {
    if (l.distinct.size < 2)
      throw new IllegalArgumentException("La lista debe tener al menos dos elementos distintos")

    l.distinct.sorted.reverse.tail.head
  }


  // 5. Escribe un programa que elimine todos los duplicados de una lista (sin usar funciones predefinidas como distinct).
  def eliminaDuplicados(l: List[Int]): List[Int] = {
    var lista2: List[Int] = List.empty

    for (i <- 0 until l.size) {
      val elem = l(i)
      if (!lista2.contains(elem))
        lista2 = lista2.appended(elem)
    }
    lista2
  }

  // Otra forma usando los elementos directamente, sin índices
  def eliminaDuplicadoss(l: List[Int]): List[Int] =
    var lista2: List[Int] = List.empty
    for (i <- l)
      if (!lista2.contains(i)) lista2 = lista2.appended(i)
    lista2


  // 6. Implementa una función que gire una lista k posiciones a la derecha (por ejemplo, [1, 2, 3, 4, 5] rotado en 2 se convierte en [4, 5, 1, 2, 3])
  def gira(l: List[Int], k: Int): List[Int] = {
    if (l.isEmpty) l
    else {
      val n = l.length
      val kMod = k % n // Aseguramos que no hacemos más rotaciones de las necesarias
      val (left, right) = l.splitAt(n - kMod)
      right ::: left
    }
  }


  // Otra forma de hacerlo
  def giraa(l: List[Int], k: Int): List[Int] = {
    val n = l.length
    if (n == 0) l
    else {
      val shift = k % n
      l.takeRight(shift) ::: l.dropRight(shift)
    }
  }

  // Si quisiesemos programar takeRight y dropRight
  def takeRight[T](l: List[T], n: Int): List[T] = {
    val len = l.length
    if (n >= len) l // Si n es mayor o igual que la longitud de la lista, devolvemos la lista completa
    else l.drop(len - n) // Eliminamos los primeros (len - n) elementos y nos quedamos con los últimos n
  }

  def dropRight[T](l: List[T], n: Int): List[T] = {
    val len = l.length
    if (n >= len) List() // Si n es mayor o igual que la longitud de la lista, devolvemos lista vacía
    else l.take(len - n) // Tomamos los primeros (len - n) elementos y descartamos los últimos n
  }


  // 8. Implementa un programa que compruebe si dos listas son permutaciones entre sí, sin ordenar las listas.
  def comprime(str: String): String = {
    if (str.isEmpty) return ""

    val resultado = new StringBuilder
    var contador = 1

    for (i <- 1 until str.length) {
      if (str(i) == str(i - 1)) {
        contador += 1 // Si el carácter es igual al anterior, aumentamos el contador
      } else {
        resultado.append(str(i - 1)).append(contador) // Guardamos el carácter y su cuenta
        contador = 1 // Reiniciamos el contador para el nuevo carácter
      }
    }

    // Agregamos el último grupo
    resultado.append(str.last).append(contador)

    resultado.toString()
  }


  // 9. Implementa un programa que encuentre el carácter que aparece con más frecuencia en una cadena (si hay más de un carácter, devolver cualquiera de ellos.
  def masFrecuente(str: String): Char = {
    if (str.isEmpty) throw new IllegalArgumentException("La cadena no puede estar vacía")

    str.groupBy(identity).mapValues(_.length).maxBy(_._2)._1

    // .maxBy(_._2) == .maxBy(tupla => tupla._2)
    // .mapValues(_.length) == .mapValues(valor => valor.length)
  }

  // 10. Crea una clase Complejo que admita sumas, restas, multiplicaciones y  módulo.
  class Complejo(val real: Double,val imaginario: Double) {
    // Suma de números complejos
    def +(otro: Complejo): Complejo =
      new Complejo(this.real + otro.real, this.imaginario + otro.imaginario)

    // Resta de números complejos
    def -(otro: Complejo): Complejo =
      new Complejo(this.real - otro.real, this.imaginario - otro.imaginario)

    // Multiplicación de números complejos
    def *(otro: Complejo): Complejo =
      new Complejo(
        this.real * otro.real - this.imaginario * otro.imaginario,
        this.real * otro.imaginario + this.imaginario * otro.real
      )

    // Módulo del número complejo
    def modulo: Double =
      Math.sqrt(this.real * this.real + this.imaginario * this.imaginario)
  }

  // 11. Implementa una función recursiva que devuelva la suma de los dígitos de un entero dado. Conviértela en recursiva de cola.
  def sumaDigitos(n:Int):Int = {
    @annotation.tailrec
    def bucle(current: Int, suma: Int): Int = {
      if (current == 0) suma
      else bucle(current / 10, suma + (current % 10))
    }
    bucle(n.abs, 0)
  }

  // 12. Define una función recursiva de cola def sumaCuadrados(List[Int]):Int que calcule la suma de los elementos de la lista de enteros que recibe como argumento.
  def sumaCuadrados(lista: List[Int]): Int = {
    @annotation.tailrec
    def bucle(resto: List[Int], acumulador: Int): Int = resto match {
      case Nil => acumulador // Caso base: cuando la lista está vacía, devuelve el acumulador
      case cabeza :: cola => bucle(cola, acumulador + cabeza * cabeza) // Llamada recursiva en cola
    }
    bucle(lista, 0) // Llamada inicial con acumulador en 0
  }

  // 13. Define una función recursiva de cola que tome un entero n mayor o igual que 0 y devuelva una lista con los números naturales desde 0 a n.
  def crearLista(n:Int):List[Int] = {
    @annotation.tailrec
    def bucle(actual: Int, l: List[Int]): List[Int] = {
      if (actual < 0) l
      else bucle(actual - 1, actual :: l)
    }
    bucle(n, List())
  }

  def main(args: Array[String]): Unit = {
    
  }
}