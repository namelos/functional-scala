import org.scalatest.{FlatSpec, Matchers}

import Lib._

class LibSpec extends FlatSpec with Matchers {
  "Partial1" can "work" in {
    val sum = (x: Int, y: Int) => x + y

    sum(1, 2) shouldBe 3

    val partialedSum = partial1(1, sum)

    partialedSum(2) shouldBe 3
  }
}