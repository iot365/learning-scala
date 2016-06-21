import scala.annotation.tailrec

// tail recursive => can be executed in constant space
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

// not tail recursive => all intermediate results must be kept!
// needs a lot of stack frames
def factorialNoTail(n: Int): Int =
  if (n == 0) 1 else n * factorialNoTail(n - 1)

factorialNoTail(4)


// do it tail recursive
def factorial(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n - 1)
  loop(1, n)
}
factorial(0)

