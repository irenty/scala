

object Inheritance extends App {
  val x = new Sub
  println(x.foo)
  println(x.bar)
}

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2
  def bar = 3
}
