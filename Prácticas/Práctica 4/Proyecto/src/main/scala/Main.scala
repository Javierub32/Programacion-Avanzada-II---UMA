import FuncionesAux.{log, thread}

import scala.util.Random
object Main {
  // Ejercicio 1.a
  def ej1_a(): Unit = {
    class Hebra(t: Int, c: Char) extends Thread {
      override def run() =
        for (i <- 0 until t)
          print(c)
    }

    val h1 = new Hebra(10, 'A')
    val h2 = new Hebra(10, 'B')
    val h3 = new Hebra(10, 'C')
    h1.start()
    h2.start()
    h3.start()
  }

  // Ejercicio 1.b
  def ej1_b(t: Int): Unit = {
    @volatile var turno = 0
    @volatile var iter = 0
    class Hebra(id: Int, times: Int, c: Char) extends Thread {
      override def run(): Unit = {
        for(i <- 0 until times) {
          while (turno != id) Thread.sleep(0)
          print(c)
          iter += 1
          if (iter == (id + 1)) {
            iter = 0
            turno = (turno + 1) % 3
          }
        }
      }
    }
    val h0 = new Hebra(0, t * 1, 'A')
    val h1 = new Hebra(1, t * 2, 'B')
    val h2 = new Hebra(2, t * 3, 'C')
    h0.start()
    h1.start()
    h2.start()
  }


  // Ejercicio 2.a
  def periodico(t: Long)(b: => Unit): Thread = {
    new Thread {
      override def run(): Unit = {
        while(true) {
          b
          Thread.sleep(t)
        }
      }
    }
  }

  // Ejercicio 2.b
  def ej2_b(): Unit = {
    val h0 = periodico(1000)(println("Pasaron 1000 milisegundos"))
    val h1 = periodico(3000)(println("Pasaron 3000 milisegundos"))
    h0.start()
    h1.start()
  }

  // Ejercicio 3.a
  def parallel[A,B](a: =>A, b: =>B):(A,B) = {
    var va = null.asInstanceOf[A]
    var vb = null.asInstanceOf[B]

    val ha = thread {
      va = a
    }

    val hb = thread {
      vb = b
    }
    ha.join()
    hb.join()
    (va,vb)
  }

  // Ejercicio 3.b
  def ej3_b(): Unit = {
    def tTrue(l: List[Boolean]): Boolean = l match {
      case Nil => true
      case true :: r => tTrue(r)
      case _ => false
    }

    val lista = List.fill(Random.nextInt(3))(Random.nextBoolean())
    log(s"$lista")
    val (l1, l2) = lista.splitAt(lista.size / 2)
    val (b1, b2) = parallel(
      tTrue(l1), tTrue(l2)
    )
    println(s"${b1 && b2}")
  }


  // Ejercicio 4
  def ej4(n: Int): Unit = {
    def fibonacci(n: Int): (Int, Int) = {
      if (n == 1) {
        println(s"${Thread.currentThread.getName}: fib(1) = 1")
        (1, 0)
      } else {
        @volatile var childRes = (0, 0)
        val child = thread {
          childRes = fibonacci(n - 1)
        }
        child.join()

        val (fn1, fn2) = childRes
        val fn = fn1 + fn2

        println(s"${Thread.currentThread.getName}: fib($n) = $fn")
        (fn, fn1)
      }

    }

    println(s"${Thread.currentThread.getName}: fib(0) = 0")
    fibonacci(n)
    println(s"${Thread.currentThread.getName}: Fin del programa.")
  }

  // Ejercicio 5.a
  def mezclar(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = (a, b) match
      case (Nil, _) => acc.reverse ::: b
      case (_, Nil) => acc.reverse ::: a
      case (ha :: ta, hb :: tb) if (ha <= hb) =>
        loop(ta, b, ha :: acc)
      case (ha :: ta, hb :: tb) if (ha >= hb) =>
        loop(a, tb, hb :: acc)

    loop(l1, l2, Nil)
  }

  // Ejercicio 5.b
  def ordenar(l: List[Int]): List[Int] = {
    if(l.isEmpty || l.size == 1) l
    else {
      val (l1, l2) = l.splitAt(l.size / 2)
      val (sol1, sol2) = parallel(
        ordenar(l1), ordenar(l2)
      )
      mezclar(sol1, sol2)
    }
  }

  def main(args: Array[String]): Unit = {
    print(ordenar(List.fill(Random.nextInt(50))(Random.nextInt(100))))
  }
}