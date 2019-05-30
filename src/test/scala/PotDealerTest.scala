package poker

class PotDealerTest extends PokerTest {

  "a pot dealer" should {

    val dealer = PotDealer.empty(List(100, 100, 100, 100)
)

    "empty dealer" in {
      Some(dealer) must bePot("100b 100 100 100!(. . . .)~!")
    }

    "build blind pots" in {
      dealer.blinds(10) must bePot("100b 95s 90B 100!(. 5 10 .)~!")
    }

    "dont allow any action before blinds" in {
      dealer.check(3) must beNone
      dealer.call(3) must beNone
    }

    "dont allow check after blinds" in {
      dealer.seq(
        _.blinds(10),
        _.check(3)) must beNone
    }

    "allow call" in {
      dealer.seq(
        _.blinds(10),
        _.call(3)
      ) must bePot("100b 95s 90B 100!(. 5 10 10)~!")
    }

  }

}
