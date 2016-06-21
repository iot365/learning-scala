abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def contains(x: Int): Boolean = false

  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(other: IntSet) = other

  override def toString = s"."

}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true


  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet) =
    ((left union right) union other) incl elem

  override def toString = s"{$left$elem$right}"
}

println("welcome to scala worksheet")
val t1 = new NonEmpty(3, Empty, Empty)
val t2 = new NonEmpty(4, t1, Empty)

if (true) 1 else false
