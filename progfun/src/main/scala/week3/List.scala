package week3

import java.util.NoSuchElementException

/**
  * Created by mike on 29.08.16.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def nth(n: Int): T =
    if (n == 0) head
    else tail.nth(n - 1)
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def nth(n: Int): T = throw new IndexOutOfBoundsException("Nil.nth")
}

object List {
  //List(1,2) = List.apply(1,2)
  def apply[T](x1 : T, x2: T) : List[T] = new Cons[T](x1, new Cons[T](x2, List(x2)))

  def apply[T](x1 : T) : List[T] = new Cons[T](x1, List())

  def apply[T]() : List[T] = new Nil[T]
}









