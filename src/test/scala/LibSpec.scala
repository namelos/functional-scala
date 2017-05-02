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
}