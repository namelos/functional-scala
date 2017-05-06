import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {
  "Option" can "map" in {
    Some(1).map(_ + 1) shouldBe Some(2)
  }

  "None" can "be mapped as None with map" in {
    None.map((x: Int) => x + 1) shouldBe None
  }

  "Get or else" can "get the value or None" in {
    Some(1).getOrElse(0) shouldBe 1
    None.getOrElse(0) shouldBe 0
  }

  "Flat map" can "map over option type" in {
    Some(1).flatMap((x: Int) => Some(x + 1)) shouldBe Some(2)
    None.flatMap((x: Int) => Some(x + 1)) shouldBe None
  }
}