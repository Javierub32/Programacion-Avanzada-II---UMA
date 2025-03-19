object Practica23 {
  // 1. Utilizando foldRight, define las funciones
  def sum(l: List[Int]): Int = l.foldRight(0)((x, acc) => x + acc) //(_+_)

  def product(l: List[Int]): Int = l.foldRight(1)((x, acc) => x * acc) //(_*_)

  def length[A](l: List[A]): Int = l.foldRight(0)((_, acc) => acc + 1)


  // 2. Utilizando foldLeft o foldRight define las funciones
  def reverse[A](l: List[A]): List[A] = {
    l.foldLeft(List[A]())((acc, x) => x :: acc)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1.foldRight(l2)((x, acc) => x :: acc)
  }


  // 3. Utilizando foldLeft o foldRight define la función
  def existe[A](l:List[A],f:A=>Boolean):Boolean = {
    l.foldLeft(false)((acc, x) => acc || f(x))
  }


  // 4. . Define la función def f(l:List[Int]):List[Int] que dada la lista l
  // construye una lista con los valores absolutos de los elementos negativos de l.
  // Por ejemplo, f(List(1,-2,3,-4,-5,6)) == List(2,4,5)
  //Implementa la función de dos formas:

  //a) Mediante una función recursiva de cola, haciendo uso del pattern matching
  def f1(l:List[Int]):List[Int] = {
    def aux(acc: List[Int], list: List[Int]):List[Int] = l match {
      case Nil => acc.reverse // No quedan elementos en la lista, devolvemos el acumulado
      case head :: tail =>
        if (head < 0) aux(math.abs(head)::acc, tail)  // Si head es < 0, lo metemos en acc
        else aux(acc, tail)  // Si es mayor que 0, lo ignoramos
    }
    aux(List(), l)
  }

  //b) Usando únicamente funciones de orden superior (map, filter, etc.)
  def f2(l: List[Int]): List[Int] = {
    l.filter(_ < 0).map(math.abs)
  }


  // 5. Usando foldRight implementa la función
  def unzip[A,B](l:List[(A,B)]):(List[A],List[B]) = (
    l.foldRight(List[A]())((x, acc) => x._1 :: acc),
    l.foldRight(List[B]())((x, acc) => x._2 :: acc)
  )


  // 6. Usando foldRight implementa la función
  def compose[A](lf: List[A => A], v: A):A = {
    lf.foldRight(v)((x , acc) => x(acc))
  }


  // 7. Usando foldRight implementa la función
  def remdups[A](lista:List[A]):List[A] = {
    lista.foldRight(List[A]())((x, acc) =>
      if (acc.isEmpty || x != acc.head) x :: acc
      else acc
    )
  }


  // 8. Usando foldRight implementa la función
  def fibonacci(n: Int): Int = {
    if (n <= 0) 0
    else if (n == 1) 1
    else (2 to n).foldRight((1, 0)) { (_, acc) =>
      (acc._1 + acc._2, acc._1)
    }._1
  }


  // 9. Usando foldRight implementa la función
  def inits[A](l: List[A]): List[List[A]] = {
    l.foldRight(List(List[A]())) { (x, acc) =>
      (x :: acc.head) :: acc
    }
  }

  // 10.Escribe una función def halfEven(l1:List[Int],l2:List[Int]):List[Int]
  // a) Mediante una función recursiva de cola, haciendo uso del pattern matching
  def halfEven(l1:List[Int],l2:List[Int]):List[Int] = {
    def bucle(l1:List[Int],l2:List[Int], acc: List[Int]): List[Int] = (l1, l2) match{
      case (_, Nil) => acc.reverse
      case (Nil, _) => acc.reverse
      case (x1 :: y1, x2 :: y2) =>
        val sum = x1 + x2
        if (sum % 2 == 0) bucle(y1, y2, sum/2 :: acc)
        else bucle(y1, y2, acc)
    }
    bucle(l1, l2, List())
  }

  // b) Usando únicamente funciones de orden superior (map, filter, etc.)
  def halfEven2(l1: List[Int], l2: List[Int]): List[Int] = {
    l1.zip(l2)
      .map { case (x1, x2) => x1 + x2 }
      .filter(_ % 2 == 0)
      .map(_ / 2)
  }

  // 11. Dada una lista de cadenas de caracteres, cada una de las cuales comienza
  // con "ERROR", "INFO" o "WARNING", queremos (1) contar el número de mensajes de
  // cada tipo y (2) extraer los mensajes de error y guárdelos en una lista.
  def logCounter(logss: List[String]): Unit = {
    val logs = List(
      "ERROR: Null pointer exception",
      "INFO: User logged in",
      "ERROR: Out of memory",
      "WARNING: Disk space low",
      "INFO: File uploaded",
      "ERROR: Database connection failed"
    )

    val counts = logs
      .groupBy(log => log.split(":")(0))
      .mapValues(_.size)
      .toMap

    val errorLogs = logs.filter(_.startsWith("ERROR"))

    println(counts)
    println(errorLogs)
  }

  
  // 12. Dada una lista de transacciones de ventas representadas como (productName, quantitySold, pricePerUnit),
  //queremos (1) calcular los ingresos totales y (2) obtener la lista de las transacciones de cuantías
  //(quantitySold) superiores (o iguales) a 100 ordenadas por su cuantía
  def salesCount(list: List[(String, Int, Double)]): Unit = {
    val sales = List(
      ("Laptop", 2, 1000.0),
      ("Mouse", 10, 15.0),
      ("Keyboard", 5, 50.0),
      ("Monitor", 3, 200.0),
      ("USB Drive", 20, 5.0)
    )

    val ej1 = sales.map((_, cantidad, precio) => cantidad * precio).sum
    val ej2 = sales
      .map { case (nombre, cantidad, precio) => (nombre, cantidad * precio) } // Calculamos el beneficio
      .filter { case (_, beneficio) => beneficio >= 100 } // Filtramos los que tienen beneficio >= 100
      .sortBy { case (_, beneficio) => -beneficio } // Ordenamos en orden descendente por beneficio

    println(ej1)
    println(ej2)
  }


  // 13. Dada una lista de oraciones, queremos extraer las palabras únicas (es decir, eliminar duplicados),
  // convertirlas a minúsculas y eliminar las palabras no significativas (como "a", "el", "es", "de", etc.).
  def uniqueWords(l: List[String]): Unit = {
    val sentences = Set(
      "Scala is a functional language",
      "The power of functional programming is great",
      "Functional programming is elegant"
    )

    val stopWords = Set("a", "the", "is", "of")

    // Procesamos las oraciones usando foldLeft
    val uniqueWords = sentences.foldLeft(Set.empty[String]) { (acc, sentence) =>
      acc ++ sentence.toLowerCase.split("\\s+").filterNot(stopWords.contains)
    }

    println(uniqueWords)
  }


  // 14. Dada una lista de palabras, queremos contar la frecuencia de cada palabra.
  def countWords(l:List[String]): Unit = {
    val wordCount = l.groupBy(identity).view.mapValues(_.size).toMap

    println(wordCount)

    // Forma imperativa
    /*
    val wordCount = l.foldLeft(Map.empty[String, Int]) { (acc, word) =>
      acc + (word -> (acc.getOrElse(word, 0) + 1))
    }
    */
  }


  // 15. Dados dos maps que representan el stock de productos en dos almacenes
  // diferentes, queremos combinarlos sumando las cantidades de los productos que aparecen en ambos.
  def sumaMapas(w1: Map[String, Int], w2: Map[String, Int]): Map[String, Int] = {
    w1 ++ w2.map { case (producto, cantidad) =>
      producto -> (cantidad + w1.getOrElse(producto, 0))
    }
    }

  def main(args: Array[String]): Unit = {
    countWords(List("hola", "s", "f", "hola", "s", "s"))
  }
}
