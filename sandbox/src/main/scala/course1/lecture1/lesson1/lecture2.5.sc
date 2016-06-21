object r {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

}


class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  def numer = x

  def denom = y

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this.<(that)) that else this


  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def - (that: Rational) = this + -that

  def unary_- : Rational = new Rational(-numer, denom)

  override def toString() = {
    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    val g: Int = gcd(x, y)
    numer / g + "/" + denom / g
  }
}


r.x.numer
r.x.denom

r.x-r.y-r.z
r.x < r.y
r.x.max(r.y)
r.x max r.y

r.x - r.x + r.y - r.y

/*val strange = {
  new Rational(1, 0)
}*/

new Rational(2)
new Rational(1,2).<(new Rational(2,3))


//strange.add(strange)

// DRY principle: don't repeat yourself
/*
((a + b) ^? (c ?^ d)) less ((a ==> b) | c)
