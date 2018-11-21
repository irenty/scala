package week3.intlist


object ConsApp extends App {
  val l = new Cons(1, new Nil)
  println(l.head)

  val l2 = new Cons(3, new Nil)
  println(l2.head)
}

trait IntList
class Cons(val head: Int, val tail: IntList) extends IntList
class Nil extends IntList

class Cons2(_head: Int, _tail: IntList) extends IntList {
  val head = _head
  val tail = _tail
}
