package poker

class SidePotTest extends PokerTest {

  "an all in should create a sidepot" should {

    val dealer = PotDealer.empty(10, List.fill(6)(100))

    val dealer2 = PotDealer.empty(10, List(100, 90, 80, 70, 60, 50))

    val dealer3 = PotDealer.empty(200, List(1000, 1000, 750))

    "update last full raise all in" in {
      dealer.seq(
        _.blinds,
        _.allin(3)
      ) must bePot("10!100b 95s 90B 0 100 100!90(. 5 10 100 . .)~!2")

    }

    "dont update last full raise when all in less than full raise" in {
      dealer2.seq(
        _.blinds,
        _.fold(3),
        _.raise(4, 35),
        _.allin(5)
      ) must bePot("10!100b 85s 70B 70 15 0!35(. 5 10 . 45 50)~!3")
    }

    "all in not enough money" in {
      dealer3.seq(
        _.blinds,
        _.raise(0, 300)
      ) must bePot("200!500b 900s 550B!300(500 100 200)~!2")

      dealer3.seq(
        _.blinds,
        _.raise(0, 300),
        _.call(1)
      ) must bePot("200!500b 500s 550B!300(500 500 200)~!2")

      dealer3.seq(
        _.blinds,
        _.raise(0, 300),
        _.call(1),
        _.allin(2)
      ) must bePot("200!500b 500s 0B!300(500 500 750)~!")
    }

    "side pots" in {
      val dealer = PotDealer.empty(2, List(50, 25, 125, 100, 75))

      val dealer2 = dealer.seq(
        _.blinds,
        _.allin(3),
        _.allin(4),
        _.allin(0),
        _.allin(1),
        _.call(2)
      )

      dealer2 must bePot("2!0b 0s 25B 0 0!98(50 25 100 100 75)~!")

      dealer2 must beSome.like {
        case d =>
          d.pots must_== List(Pot(125, List(0, 1, 2, 3, 4)),
            Pot(100, List(0, 2, 3, 4)),
            Pot(75, List(2, 3, 4)),
            Pot(50, List(2, 3))
          )
      }
    }

    "side pots with folds" in {
      val dealer = PotDealer.empty(2, List(50, 25, 125, 100, 75))

      val dealer2 = dealer.seq(
        _.blinds,
        _.call(3),
        _.call(4),
        _.allin(0),
        _.allin(1),
        _.call(2),
        _.fold(3),
        _.call(4)
      )

      dealer2 must bePot("2!0b 0s 75B 98 25!48(50 25 50 2 50)~!")

      dealer2 must beSome.like {
        case d =>
          d.pots must_== List(
            Pot(102, List(0, 1, 2, 4)),
            Pot(75, List(0, 2, 4))
          )
      }

    }
  }
}
