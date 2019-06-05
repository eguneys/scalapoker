package poker

class PotDealerTest extends PokerTest {

  "a pot dealer" should {

    val dealer = PotDealer.empty(List(100, 100, 100, 100)
)

    "empty dealer" in {
      Some(dealer) must bePot("100b 100 100 100!0(. . . .)~!0")
    }

    "build blind pots" in {
      dealer.blinds(10) must bePot("100b 95s 90B 100!10(. 5 10 .)~!3")
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
      ) must bePot("100b 95s 90B 90!10(. 5 10 10)~!3")
    }

    "allow call when already money in the pot" in {
      dealer.seq(
        _.blinds(10),
        _.call(3),
        _.call(0),
        _.call(1)
      ) must bePot("90b 90s 90B 90!10(10 10 10 10)~!3")
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

        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.call(0),
          _.raise(1, 9)
        ) must beNone
      }

      "allow raise of blind" in {
        dealer.seq(
          _.blinds(10),
          _.raise(3, 10)
        ) must bePot("100b 95s 90B 80!10(. 5 10 20)~!3")
      }
    }

    "fold" should {
      "allow raise after fold" in {
        // b s B .
        // CR R F CR
        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.call(0),
          _.raise(1, 10),
          _.fold(2),
          _.call(3),
          _.raise(0, 10)
        ) must bePot("70b 80s 90B 80!10(30 20 10 20)~!0")
      }
    }

    "allin" should {
      "reopen betting action" in {
        // b s B .
        // A . . C
        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.allin(0)
        ) must bePot("0b 95s 90B 90!90(100 5 10 10)~!0")
      }

      "dont reopen betting action" in {
        val dealer = PotDealer.empty(List(5, 100, 100, 100))

        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.allin(0)) must bePot("0b 95s 90B 90!10(5 5 10 10)~!3")

        val dealer2 = PotDealer.empty(List(19, 100, 100, 100))

        dealer2.seq(
          _.blinds(10),
          _.call(3),
          _.allin(0)) must bePot("0b 95s 90B 90!10(19 5 10 10)~!3")
      }

      "dont close current betting action when allin" in {

        val dealer2 = PotDealer.empty(List(19, 100, 100, 100))

        dealer2.seq(
          _.blinds(10),
          _.call(3),
          _.allin(0)) must bePot("0b 95s 90B 90!10(19 5 10 10)~!3")

      }


    }
  }

}
