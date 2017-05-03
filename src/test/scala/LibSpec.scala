import org.scalatest.{FlatSpec, Matchers}

import Lib._

class LibSpec extends FlatSpec with Matchers {

  val sum = (x: Int, y: Int) => x + y

  "Partial1" can "work" in {
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

  "Curry" can "curry a function" in {
    val curriedSum = curry(sum)

    curriedSum(1)(2) shouldBe 3
  }

  "Uncurry" can "uncurry a function" in {
    val uncurriedSum = uncurry(curry(sum))

    uncurriedSum(1, 2) shouldBe 3
  }

  "Compose" can "compose functions" in {
    val even = (x: Int) => x % 2 == 0
    val length = (x: String) => x.length

    val fn = compose(even, length)

    fn("function") shouldBe true
    fn("compose") shouldBe false
  }

  "List" can "be constructed with conses" in {
    val tuple = List(1, 2, 3) match {
      case Cons(x, Cons(y, Cons(z, Nil))) => (x, y, z)
      case _ =>
    }

    tuple shouldBe (1, 2, 3)
  }

  "Sum" can "get sum of a list" in {
    List.sum(List(1, 2, 3, 4)) shouldBe 10
  }

  "Product" can "get the product of a list" in {
    List.product(List(1, 2, 3, 4)) shouldBe 24
  }

  "Tail" can "get the tail of a list" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  "Set head" can "set the head of a list" in {
    List.setHead(0, List(1, 2, 3)) shouldBe List(0, 2, 3)
  }

  "Drop" can "drop first nth elements" in {
    List.drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
  }

  "Map" can "map over all list elements" in {
    List.map(List(1, 2, 3), (x: Int) => x * 10) shouldBe List(10, 20, 30)
  }

  "Drop while" can "drop element in list while it is qualified" in {
    List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x % 2 == 0) shouldBe List(1, 3, 5)
  }

  "Append" can "append a second list to the first one" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Init" can "get the initial part of a list" in {
    List.init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  "Curried drop while" can "infer callback parameter type" in {
    List.curriedDropWhile(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(1, 3, 5)
  }

  "Sum with fold right" can "get the sum of a list" in {
    List.sumFoldR(List(1, 2, 3, 4)) shouldBe 10
  }

  "Product with fold right" can "get the product of a list" in {
    List.productFoldR(List(1, 2, 3, 4)) shouldBe 24
  }
}