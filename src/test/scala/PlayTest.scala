package poker

import format.Visual.addNewLines

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {
      val game = makeGame(List(100, 100))

      "before post blinds" in {
        addNewLines(game.board.visual) must_== """
100b 100!(. .)~!
"""
      }

      "players check" in {
        "current game" in {
          val game2 = game.deal(10).get.playActs(
            Check,
            Check,

            Check)

          game2 must beSome.like {
            case g => addNewLines(g.board.visual) must_== """
95b 90B!(5 10)~!
H
H H
"""
          }
        }
      }
    }
  }
}
