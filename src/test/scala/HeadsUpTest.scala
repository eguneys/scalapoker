package poker

class HeadsUpTest extends PokerTest {

  "heads up play" should {

    val game = makeGame(10, List(100, 100))
    val headsup = game.deal.get

    "should deal cards" in {
      Some(headsup) must beGame("""
10!95b 90B!10(5 10)~!1
""")
    }

    "before posting blinds" should {
      "dont allow any action" in {
        Some(game) must bePoss()
      }
    }

    "on pre flop" should {

      "dont allow check" in {
        Some(headsup) must bePoss(Call, Fold, AllIn)
        Some(headsup) must bePossRaise(Raise(10))
      }

      "players call check" in {
        headsup.playActs(Call) must bePoss(Check, Fold, AllIn)
        headsup.playActs(Call) must bePossRaise(Raise(10))

        headsup.playActs(Call) must beGame("""
10!90b 90B!10(10 10)~!1
C
""")

        headsup.playActs(Call, Check) must beGame("""
10!90b 90B!10(10 10)~!0

H C
""")
      }

      "small blind fold" in {
        headsup.playActs(Fold) must bePoss()

        headsup.playActs(Fold) must beGame("""
10!95b 90B!10(5 10)~!1
F
""")
      }
    }


    "after rounds end" in {
      headsup.playActs(Fold) must beSome.like {
        case game =>
          game.endRounds map(_._1) must beGame("""
10!95 105b!0(. .)~!
""")
      }
    }

    "all in " in {
      headsup.playActs(AllIn) must beGame("""
10!0b 90B!90(100 10)~!1
A
""")
    }
  }

}
