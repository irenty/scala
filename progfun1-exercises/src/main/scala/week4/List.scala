package week4

package week3


object ConsApp extends App {
  val l = new Cons(1, new Cons(4, new Nil))
  println(l.head)

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  val n = singleton(1)
  println(n)
  val b = singleton(true)
  println(b)

  println(l.nth(1))

  println(l.nth(1, l))
}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T

  def nth(n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n <= 0) xs.head
    else nth(n-1, xs.tail)
}

object List {
  def apply[T](): List[T] = new Nil
  def apply[T](x1: T): List[T] = new Cons(x1, List())
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  // or: def apply[T](x1: T, x2: T): List[T] = new Cons(x1, List(x2))

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def nth(n: Int): T = {
    def getNth(xs: List[T], myIndex: Int): T = {
      if (xs.isEmpty) throw new NoSuchElementException("Nil.head")
      if (myIndex == n) return xs.head
      getNth(xs.tail, myIndex + 1)
    }
    getNth(this, 0)
  }
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  def head = throw new NoSuchElementException("Nil.head")

  def tail = throw new NoSuchElementException("Nil.tail")

  override def nth(n: Int): T = throw new NoSuchElementException("Nil.tail")
}
