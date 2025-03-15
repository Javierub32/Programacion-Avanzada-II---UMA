package practica_22


class EfficientQueue[T] private (private val front: List[T], private val rear: List[T]) extends ImmutableQueue[T] {
  def this() = this(Nil,Nil)

  def this(elems: T*) = this(elems.toList, Nil)

  def enqueue(elem: T): ImmutableQueue[T] = {
    new EfficientQueue[T](front, elem::rear)
    // añadimos el elemento al principio de rear,
    // es decir, al final de la cola
  }

  def dequeue(): (T, ImmutableQueue[T]) = {
    front match {
      case Nil =>
        // si front vacío -> invertimos rear y lo
        // metemos a front
        rear.reverse match {
          case Nil => throw new NoSuchElementException("Cola vacía")
          case head :: tail => (head, new EfficientQueue[T](tail, Nil))
          // sacamos el primer elemento y nos quedamos con el resto de la
          // lista
        }
      case head::tail => (head,new EfficientQueue[T](tail,rear))
      // sacamos el primer elemento del front y dejamos rear tal
      // como está
    }
  }

  def isEmpty: Boolean = front.isEmpty && rear.isEmpty

  override def toString: String = {
    val elements = front ::: rear.reverse
    elements.mkString(" SimpleQueue(List(",", ","))")
    //s"SimpleQueue(List(${elements.mkString(", ")}))"
    // Otra manera de dar formato, ambas funcionan
  }
  override def equals(obj: Any): Boolean = {
    obj match
      case that: EfficientQueue[T] =>
        (this.front:::this.rear.reverse) == (that.front:::that.rear.reverse)
      // La concatenación de front y rear invertido, es decir, lo que sería
      // la cola al completo, debe ser igual
      case _ => false
  }

  override def hashCode(): Int = (front:::rear.reverse).hashCode()
  // Buscamos el hashCode de la cola entera, por ello hay que concatenar ambas partes
}


@main def testImmutableQueue(): Unit = {
  val squeue = new EfficientQueue[Int]()
  val q = squeue.enqueue(1).enqueue(2).enqueue(3).enqueue(4)
  assert(q.dequeue() == (1, EfficientQueue(2, 3, 4)), s"${q.dequeue()} should be equal to (1, SimpleQueue(List(2, 3, 4)))")
  assert(squeue.isEmpty, s"{q} should be empty")
  assert(!q.isEmpty, s"{q should not be empty")
  val q2 = EfficientQueue(1, 2, 3, 4)
  assert(q == q2, s"${q} and ${q2} should be equal")
  assert(q.hashCode() == q2.hashCode(), s"The hash codes of ${q} and ${q2} should be equal")
}
