package week5


object ConsApp extends App {

  def flatten(xs: List[Any]): List[Any] =
    if (xs.isEmpty) List.empty
    else xs.head match {
        case l: List[Any] => flatten(l) ::: flatten(xs.tail)
        case element: Any => element :: flatten(xs.tail)
      }

  val f = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  println(f)
  assert(f == List(1, 1, 2, 3, 5, 8))
}
