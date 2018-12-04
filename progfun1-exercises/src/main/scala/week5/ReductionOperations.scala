package week5

object ConsApp62 extends App {

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( (l, _) => List(f(l))  )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (_, right) => right + 1 )

  def myFlatten(xss: Seq[Seq[Int]]): Seq[Int] =
    (xss foldRight Seq[Int]())(_ ++ _)

  val s = Seq(Seq(1, 2, 3), Seq(4, 5))

  println(s.flatten)
  println(myFlatten(s))
}
