package coursera.fpis.classes

object intsets {
}

abstract class IntSets {
  def incl(x: Int): IntSets
  def contains(x: Int): Boolean
}

