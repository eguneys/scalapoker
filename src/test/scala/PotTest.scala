package poker

class PotTest extends PokerTest {

  "a pot dealer" should {

    val dealer = PotDealer.empty(List.fill(6)(100))

    "a running pot" should {

      val onepot = dealer.seq(
        _.blinds(10),
        _.call(3),
        _.call(4),
        _.call(5),
        _.call(0),
        _.call(1),
        _.check(2)).get

      "distribute pot" in {
        onepot.distribute(Nil, List(1, 2, 3, 4, 5, 6)) must bePot("90 90b 90 90 90 150!0(. . . . . .)~!")

        onepot.distribute(Nil, List(6, 5, 4, 3, 2, 1)) must bePot("150 90b 90 90 90 90!0(. . . . . .)~!")
      }

      "money in the pot folds" in {
        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.call(4),
          _.fold(5),
          _.fold(0),
          _.fold(1),
          _.check(2),
          _.distribute(List(5, 0, 1), List(6, 5, 4, 3, 2, 1))) must bePot("100 95b 125 90 90 100!0(. . . . . .)~!")


      }

      "some folds" in {
        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.fold(4),
          _.fold(5),
          _.fold(0),
          _.call(1),
          _.check(2),
          _.distribute(List(4, 5, 0), List(6, 5, 4, 3, 2, 1))) must bePot("100 120b 90 90 100 100!0(. . . . . .)~!")

        dealer.seq(
          _.blinds(10),
          _.call(3),
          _.fold(4),
          _.fold(5),
          _.fold(0),
          _.call(1),
          _.check(2),
          _.distribute(List(4, 5, 0), List(1, 2, 3, 4, 5, 6))) must bePot("100 90b 90 120 100 100!0(. . . . . .)~!")
      }
    }

  }

}
