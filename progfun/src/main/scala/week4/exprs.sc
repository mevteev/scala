import week4.{Prod, Var}

object exprs {

  import week4.Expr
  import week4.Number
  import week4.Sum

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1 , e2) => show(e1) + " + " + show(e2)
    case Var(v) => v
    case Prod(e1, e2) => {
      val isSum1 = e1.isInstanceOf[Sum]
      val isSum2 = e2.isInstanceOf[Sum]

      (if(isSum1) "(" else "") +
        show(e1) +
        (if(isSum1) ")" else "") +
        " * " +
        (if(isSum2) "(" else "") +
        show(e2) +
        (if(isSum2) ")" else "")
    }
  }

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }


  eval(Sum(Number(1), Number(8)))

  show(Sum(Number(4), Number(88)))

  show(Prod(Sum(Number(5), Number(8)), Prod(Number(1), Var("x"))))

  show(Sum(Prod(Number(2), Var("x")), Var("y")))

  show(Prod(Sum(Number(2), Var("x")), Var("y")))


}
