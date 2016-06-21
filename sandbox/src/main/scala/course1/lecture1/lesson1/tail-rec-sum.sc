object exercise {
  def sum1(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  // currying

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f)(a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def product2(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }
    loop(a, 1)
  }

  def fact(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }
}

exercise.sum(x => x)(1, 100)
exercise.product(x => x * x)(3, 4)
exercise.product2(x => x * x)(3, 4)
exercise.fact(5)

def product(f: Int => Int)(a: Int, b: Int): Int = exercise.mapReduce(f, (g, h) => g * h, 1)(a, b)

product(x => x * x)(3, 4)

