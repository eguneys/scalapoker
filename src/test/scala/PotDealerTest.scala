package poker

class PotDealerTest extends PokerTest {

  "a pot dealer" should {

    val dealer = PotDealer.empty(List(100, 100, 100, 100)
)

    "empty dealer" in {
      Some(dealer) must bePot("100b 100 100 100!(. . . .)~!")
    }

    "build blind pots" in {
      dealer.blinds(10) must bePot("100b 95s 90B 100!(0 5 10 0)~!")
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
      ) must bePot("100b 95s 90B 90!(0 5 10 10)~!")
    }

    "allow call when already money in the pot" in {
      dealer.seq(
        _.blinds(10),
        _.call(3),
        _.call(0),
        _.call(1)
      ) must bePot("90b 90s 90B 90!(10 10 10 10)~!")
    }

    "find is settled" should {
      "not settled before blinds" in {
        dealer.isSettled must beFalse
      }

      "not settled after blinds" in {
        dealer.blinds(10) must beSome.like {
          case d =>
            d.isSettled must beFalse
        }
      }

      "not settled after a call" in {
        dealer.seq(
          _.blinds(10),
          _.call(3)
        ) must beSome.like {
          case d =>
            d.isSettled must beFalse
        }

        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.call(0)
        ) must beSome.like {
          case d =>
            d.isSettled must beFalse
        }
      }

      "settled after all players call" in {
        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.call(0),
          _.call(1)
        ) must beSome.like {
          case d =>
            d.isSettled must beTrue
        }
      }
    }

    "raise" should {
      "dont allow raise smaller than blind" in {
        dealer.seq(
          _.blinds(10),
          _.raise(3, 9)
        ) must beNone
      }

      "allow raise of blind" in {
        dealer.seq(
          _.blinds(10),
          _.raise(3, 10)
        ) must bePot("100b 95s 90B 80!(0 5 10 20)~!")
      }
    }
  }

}
