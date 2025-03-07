object Main {
  def main(args: Array[String]): Unit =
    println("Hello friend! " + (args mkString ","));

  def hola(n:Int):Int =
    var x = 1
    println(x)
    x
}

class Practicas {
  def primeFactors(n:Int): List[Int] = {
    def bucle(current: Int, divisor: Int, l: List[Int]): List[Int] =
      // Si hemos llegado a 1, ya no hay más factores que extraer
      if (current == 1) l
      else if (current % divisor == 0)
        // Si 'divisor' es factor de 'current', lo añadimos
        // y seguimos factorizando con el mismo divisor
        bucle(current / divisor, divisor, l :+ divisor)
      else if (divisor * divisor > current)
        // Si el divisor ya supera la raíz cuadrada de 'current'
        // entonces 'current' es primo y lo añadimos
        l :+ current
      else {
        // Si no es divisible, incrementamos el divisor y seguimos
        bucle(current, divisor + 1, l)
      }

    bucle(n, 2, Nil)
  }

  def binarySearch (arr: Array[Int], elt: Int): Option[Int] = {
    def bucle(l: Array[Int], n:Int, inf:Int, sup:Int): Option[Int] = {
      if (inf <= sup) {
        val medio = (inf + sup) / 2
        if (l(medio) == n) Some(medio)
        else if (l(medio) < n) bucle(l, n, medio + 1, sup) // Busco arriba
        else bucle(l, n, inf, medio - 1) // Busco abajo
      }
      else None
    }
    bucle(arr, elt, 0, arr.length - 1);
  }

  def unzip[A, B](l: List[(A, B)]): (List[A], List[B]) = l match {
    case Nil => (Nil, Nil)
    case (a, b) :: tail =>
      val (as, bs) = unzip(tail)
      (a :: as, b :: bs)
  }

  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case head :: tail =>
      if (f(head)) head :: filter(tail, f)
      else filter(tail,f)
  }

  def map[A,B](l : List[A], f: A => B): List[B] = l match {
    case Nil => Nil
    case head :: tail => f(head) :: map(tail, f)
  }

  def groupBy[A, B](l: List[A], f: A => B): Map[B, List[A]] = l match {
    case Nil => Map.empty
    case head :: tail =>
      val clave = f(head)             // Me devuelve la clave
      val resto = groupBy(tail, f)    // Me devuelve el mapa actual
      resto + (clave -> (head :: resto.getOrElse(clave, Nil)))
      // Añado (o machaco) en el mapa acual la clave del head actual preponiendole el head actual a la lista cuya clave sea la del head.
  }
}