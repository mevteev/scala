object bool {

  abstract class Boolean_ {

    object true_ extends Boolean_ {
      def ifThenElse[T](t: => T, e: => T): T = t
    }

    object false_ extends Boolean_ {
      override def ifThenElse[T](t: => T, e: => T): T = e
    }


    def ifThenElse[T](t: => T, e: => T): T

    def &&(x: => Boolean_): Boolean_ = ifThenElse(x, false_)

    def ||(x: => Boolean_): Boolean_ = ifThenElse(true_, x)

    def unary_!(): Boolean_ = ifThenElse(false_, true_)

    def ==(x: => Boolean_) = ifThenElse(x, x.unary_!)

    def !=(x: => Boolean_) = ifThenElse(x.unary_!, x)

    def <(x: => Boolean_) = ifThenElse(false_, x)

  }

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that:Nat) :Nat
    def - (that:Nat) :Nat
  }

  object Zero extends Nat {
    override def isZero: Boolean = true

    override def predecessor: Nat = throw new Exception

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = if (!that.isZero) throw new Exception else this
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def predecessor: Nat = n

    override def +(that: Nat): Nat = new Succ(n + that)

    override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
  }




}









