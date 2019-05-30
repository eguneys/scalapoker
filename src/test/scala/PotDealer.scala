package poker

class PotDealerTest extends PokerTest {

  "a pot dealer" should {

    val dealer = PotDealer.empty(List(100, 100, 100, 100))

    "build blind pots" in {
      dealer.blinds(1, 2, 10) must bePot("100b 95 90 100!(0 5 10 0)~!")
    }

    // "dont allow check" in {
    //   // dealer.check(3) must beNone
    // }

  }

}
