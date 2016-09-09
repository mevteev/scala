object session {
  1 + 2

  def abs(x: Double) = if (x < 0) -x else x


  def sqrt(x: Double) = {


    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)

  }

  sqrt(2)
  sqrt(4)
  sqrt(1e60)

  def factorial(x: Int) : Int =
    if (x == 0) 1
    else x * factorial(x - 1)

  def fact(x: Int) : Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, x)
  }



  fact(4)


}


// 5! = 1 * 2 * 3 * 4 * 5