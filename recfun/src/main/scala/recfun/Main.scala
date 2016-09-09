package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(acc: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) acc == 0
      else {
        if (chars.head == '(') loop(acc + 1, chars.tail)
        else {
          if (chars.head == ')') {
            if (acc == 0) false
            else loop(acc - 1, chars.tail)
          }
          else loop(acc, chars.tail)
        }
      }
    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(acc: Int, coin: Int, n: Int, coins: List[Int]): Int = {
      if (n - coin <= 0) acc
      else {
        loop(acc + countChange(n - coin, coins), coin, n - coin, coins)
      }
    }

    if (coins.isEmpty || money == 0) 0
    else {
      if (money < coins.head) countChange(money, coins.tail)
      else {
        if (money % coins.head == 0) 1 else 0
      } +
        loop(0, coins.head, money, coins.tail) +
        countChange(money, coins.tail)
    }
  }
}
