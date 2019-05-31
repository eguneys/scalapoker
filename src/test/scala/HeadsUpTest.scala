package poker

class HeadsUpTest extends PokerTest {

  "heads up play" should {

    val game = makeGame(List(100, 100))
    val headsup = game.deal(10).get

    "should deal cards" in {
      Some(headsup) must beGame("""
95b 90B!(5 10)~!
""")
    }

    "before posting blinds" should {
      "dont allow any action" in {
        Some(game) must bePoss()
      }
    }

    "on pre flop" should {

      "dont allow check" in {
        Some(headsup) must bePoss(Call, Fold)
        Some(headsup) must bePossRaise(Raise(5))
      }

      "players call check" in {
        headsup.playActs(Call) must bePoss(Check, Fold)
        headsup.playActs(Call) must bePossRaise(Raise(5))

        headsup.playActs(Call) must beGame("""
90b 90B!(10 10)~!
C
""")

        headsup.playActs(Call, Check) must beGame("""
90b 90B!(10 10)~!

H C
""")
      }
    }
  }

}
