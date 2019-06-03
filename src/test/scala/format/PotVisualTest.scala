package poker
package format

class PotVisualTest extends PokerTest {

  val f = PotVisual

  "The visual pot formatter" should {
    "export pot" in {
      val pot = PotDealer.empty(List(10, 10))
      val pot2 = PotDealer(1, true, List(9, 10, 0, 0),
        PotBuilder(10, Map(0 -> 20, 1 -> 10),
          Set(0, 1, 2, 3)))

      f >> pot must_== "10b 10!0(. .)~!"
      f << (f >> pot) must_== pot

      f >> pot2 must_== "9 10b 0s 0B!10(20 10 . .)~!"
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

  val newPotFormat = "10b 10!0(. .)~!"

  val examples = Seq(
    newPotFormat,
    "10 10b 10!0(. . .)~!",
    "9b 10 0 0!0(. 10 20 30)~!",
    "9 10 0 0b!10(. 10 . .)~!",
    "9 10b 0 0!10(5 10 . .)~!",
    "10B 10b 10s!10(. . .)~!",
    "9b 10s 0B 0!10(. 10 20 30)~!",
    "9s 10B 0 0b!0(. 10 . .)~!",
    "9 10b 0s 0B!10(5 10 . .)~!"
  )
}
