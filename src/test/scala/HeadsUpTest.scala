package poker

import format.Visual.addNewLines

class HeadsUpTest extends PokerTest {

  "heads up play" should {

    val game = makeGame(List(100, 100))
    val headsup = game.deal(10).get

    "should deal cards" in {
      addNewLines(headsup.board.visual) must_== """
95b 90
. .
"""
    }

    "before posting blinds" should {
      "dont allow any action" in {
        game must bePoss()
      }
    }

    "on pre flop" should {

      "dont allow check" in {
        headsup must bePoss(Call, Fold)
      }

      "both players check" in {
        headsup.playActs(Check) must beSome.like {
          case headsup =>
            addNewLines(headsup.board.visual) must_== """
95b 90
C .
"""
        }

        headsup.playActs(Check, Check) must beSome.like {
          case headsup =>
            addNewLines(headsup.board.visual) must_== """
95b 90
. .
C C
"""
        }
      }
    }
  }

}
