object sessionSqrt {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

}


sessionSqrt.sqrt(2)
sessionSqrt.sqrt(4)
sessionSqrt.sqrt(1e-6)
sessionSqrt.sqrt(1e60)
sessionSqrt.sqrt(0.001)
sessionSqrt.sqrt(1e-20)
sessionSqrt.sqrt(1e20)
sessionSqrt.sqrt(1e50)


object sessionSqrtBlock {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }

}

sessionSqrtBlock.sqrt(2)
sessionSqrtBlock.sqrt(4)
sessionSqrtBlock.sqrt(1e-6)
sessionSqrtBlock.sqrt(1e60)
sessionSqrtBlock.sqrt(0.001)
sessionSqrtBlock.sqrt(1e-20)
sessionSqrtBlock.sqrt(1e20)
sessionSqrtBlock.sqrt(1e50)

object sessionSqrtBlockSimplified {
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

}

sessionSqrtBlockSimplified.sqrt(2)
sessionSqrtBlockSimplified.sqrt(4)
sessionSqrtBlockSimplified.sqrt(1e-6)
sessionSqrtBlockSimplified.sqrt(1e60)
sessionSqrtBlockSimplified.sqrt(0.001)
sessionSqrtBlockSimplified.sqrt(1e-20)
sessionSqrtBlockSimplified.sqrt(1e20)
sessionSqrtBlockSimplified.sqrt(1e50)

