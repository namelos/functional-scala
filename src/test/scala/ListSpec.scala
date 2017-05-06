import org.scalatest.{FlatSpec, Matchers}

import List._

class ListSpec extends FlatSpec with Matchers {

  val sum = (x: Int, y: Int) => x + y

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
    product(List(1, 2, 3, 4)) shouldBe 24
  }

  "Tail" can "get the tail of a list" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  "Set head" can "set the head of a list" in {
    setHead(0, List(1, 2, 3)) shouldBe List(0, 2, 3)
  }

  "Drop" can "drop first nth elements" in {
    drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
  }

  "Map" can "map over all list elements" in {
    map(List(1, 2, 3), (x: Int) => x * 10) shouldBe List(10, 20, 30)
  }

  "Drop while" can "drop element in list while it is qualified" in {
    dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x % 2 == 0) shouldBe List(1, 3, 5)
  }

  "Append" can "append a second list to the first one" in {
    append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Init" can "get the initial part of a list" in {
    init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  "Curried drop while" can "infer callback parameter type" in {
    curriedDropWhile(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(1, 3, 5)
  }

  "Sum with fold right" can "get the sum of a list" in {
    sumFoldR(List(1, 2, 3, 4)) shouldBe 10
  }

  "Product with fold right" can "get the product of a list" in {
    productFoldR(List(1, 2, 3, 4)) shouldBe 24
  }

  "Fold right" can "keep the data same when given Nil" in {
    foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3, 4)
  }

  "Length" can "calculate the length of a list" in {
    lengthFoldR(List(1, 2, 3)) shouldBe 3
  }

  "Fold left" can "fold through a whole list" in {
    foldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  "Sum fold left" can "get the sum of a list" in {
    sumFoldL(List(1, 2, 3, 4)) shouldBe 10
  }

  "Product with fold left" can "get the product of a list" in {
    productFoldL(List(1, 2, 3, 4)) shouldBe 24
  }

  "Length" can "get the length of a list" in {
    lengthFoldL(List(1, 2, 3)) shouldBe 3
  }

  "Reverse" can "reverse a list" in {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  "Fold right with fold left" can "fold through a whole list" in {
    foldRWithFoldL(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  "Append with fold left" can "append a second list to the first one" in {
    appendWithFold(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Concat" can "merge all lists" in {
    concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  "Map increment" can "increment all element in list" in {
    mapIncrement(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  "Map double to string" can "map all double to string" in {
    mapDoubleToString(List(0.0, 1.0, 2.0)) shouldBe List("0.0", "1.0", "2.0")
  }

  "Map" can "map all element in list over a function" in {
    mapWithFold(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
  }

  "Filter" can "filter all qualified elements" in {
    filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "Flat map" can "map all element to lists and then concat all lists" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "Filter with flat map" can "filter all qualified elements" in {
    filterWithFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "Sum two lists" can "sum elements in two lists respectively and return a new list" in {
    sumTwoLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }
}

