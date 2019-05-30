package poker
package format

class PotVisualTest extends PokerTest {

  val f = PotVisual

  "The visual pot formatter" should {
    "export pot" in {
      val pot = PotDealer(0, List(10, 10), PotBuilder(Map.empty[StackIndex, Int]), Nil)
      val pot2 = PotDealer(1, List(9, 10, 0, 0),
        PotBuilder(Map(0 -> 20, 1 -> 10)),
        List(Pot(10, List(0, 1, 2)),
          Pot(5, List(0, 1))))

      f >> pot must_== "10b 10!(. .)~!"
      f << (f >> pot) must_== pot

      f >> pot2 must_== "9 10b 0 0!(20 10 . .)~!10 (0 1 2)!5 (0 1)"
      f << (f >> pot2) must_== pot2
    }
  }

  "The visual pot formatter" should {
    "export pot" in {
      f >> (PotDealer.empty(List(10, 10))) must_== newPotFormat
    }

    "import and export is non destructive" in {
      forall(examples) { example =>
        f >> (f << example) must_== example
      }
    }
  }

  val newPotFormat = "10b 10!(. .)~!"

  val examples = Seq(
    newPotFormat,
    "10 10b 10!(. . .)~!",
    "9b 10 0 0!(. 10 20 30)~!",
    "9 10 0 0b!(. 10 . .)~!10 (0 1 2)",
    "9 10b 0 0!(5 10 . .)~!10 (0 1 2)!5 (0 1)"
  )
}
