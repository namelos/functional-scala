import org.scalatest.{FlatSpec, Matchers}

import Lib._

class LibSpec extends FlatSpec with Matchers {
  "Partial1" can "work" in {
    val sum = (x: Int, y: Int) => x + y

    sum(1, 2) shouldBe 3

    val partialedSum = partial1(1, sum)

    partialedSum(2) shouldBe 3
  }

  "Factorial" can "calculate factorial n" in {
    factorial(1) shouldBe 1
    factorial(2) shouldBe 2
    factorial(5) shouldBe 120
  }

  "Fib" can "calculate fibonacci n" in {
    fib(1) shouldBe 1
    fib(2) shouldBe 1
    fib(5) shouldBe 5
    fib(6) shouldBe 8
    fib(10) shouldBe 55
  }

  "Find first" can "find first qualified item index" in {
    findFirst(Array("one", "two", "three"), (x: String) => x == "two")

    findFirst(Array(1, 2, 3), (x: Int) => x == 1)
  }

  "Is sorted" can "check if an array is sorted" in {
    isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y) shouldBe true

    isSorted(Array(1, 2, 1), (x: Int, y: Int) => x <= y) shouldBe false
  }
}