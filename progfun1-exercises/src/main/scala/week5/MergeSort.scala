package week5

object ConsApp2 extends App {

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.size / 2
    if (n == 0) xs // xs.size =  0 or 1
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }


      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  val sorted = msort(List(2, -4, 5, 7))
  println(sorted)

}
