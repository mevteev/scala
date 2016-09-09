object lists {

  def isort(xs: List[Int]) : List[Int] = {
    xs match {
      case List() => List()
      case y :: ys => insert(y, isort(ys))
    }
  }

  def insert(y: Int, ys: List[Int]) : List[Int] = {
    ys match {
      case List() => List(y)
      case x :: xs =>
        if (y < x) y :: ys
        else x :: insert(y, xs)
    }
  }


  isort(List(4,7,6,1, 4, 5, 7, 2))
}