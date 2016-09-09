object rationals {

  class Rational(x: Int, y:Int) {
    def numer = x
    def denom = y

    def add(that:Rational) : Rational =
      new Rational(numer * that.denom + denom * that.numer,denom * that.denom)

    def neg : Rational = new Rational(-numer, denom)

    def - (that : Rational) : Rational = add(that.neg)


    override def toString() =
      numer + "/" + denom
  }


  val x = new Rational(1, 2)

  val y = new Rational(2, 3)



  x.add(y)

  x - y

  x.neg

  x add y




}

