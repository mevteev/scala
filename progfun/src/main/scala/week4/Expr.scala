package week4

/**
  * Created by mike on 31.08.16.
  */
trait Expr {



}

/*
object Number {
  def apply(n: Int) = new Number(n)
}

object Sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}
*/

case class Number (n:Int) extends Expr

case class Sum (e1:Expr, e2:Expr) extends Expr

case class Var (s: String) extends Expr

case class Prod(e1: Expr, e2:Expr) extends Expr
