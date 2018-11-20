package week3.intsets2

// Empty as Singleton
object IntSets2 extends App {

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  val t3 = t2 incl 4
  println(t1)
  println(t2)
  println(t3)

  val s1 = new NonEmpty(5, Empty, Empty)
  val s2 = s1 incl 8
  val s3 = s2 incl 2
  val s4 = s3 incl 4

  val u = t2 union s4
  println(u)
}

abstract class IntSet {

  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet

}

object Empty extends IntSet {

  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString = "."

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x) else this

  override def toString = s"{ $left $elem $right}"

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}
