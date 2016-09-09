import intsets.{Number, Sum}
import week3.{Cons, Nil}

object intsets {
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  val t5 = new NonEmpty(5, Empty, Empty)
  val t6 = t5 incl 1
  val t3 = t2 union t6


  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int): Boolean = false

    override def toString = "."

    override def union(other: IntSet): IntSet = other
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this

    override def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem

    override def toString = "{" + left + elem + right + "}"
  }

  type A = IntSet => NonEmpty
  type B = NonEmpty => IntSet




  object Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => eval(e1) + eval(e2)
    }

    def show: String = this match {
      case Number(n) => n.toString
      case Sum(e1, e2) => show(e1) + " + " + show(e2)
    }

    show(Sum(Number(4), Number(88)))


  }


  trait Expr {
  }


  case class Number(n:Int) extends Expr {
  }

  case class Sum(e1:Expr, e2:Expr) extends Expr {
  }

  object Number {
    def apply(n: Int) = new Number(n)
  }

  object Sum {
    def apply(e1:Expr, e2:Expr ) = new Sum(e1, e2)
  }



}







