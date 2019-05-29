package poker

import format.Visual.addNewLines

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {
      val game = makeGame(List.fill(2)(10))

      "post blinds" in {
        addNewLines(game.board.visual) must_== """
10b 10
. .
"""
      }

      "players check" in {
        "current game" in {
          val game2 = game.playActs(
            Check,
            Check,

            Check)

          game2 must beSome.like {
            case g => addNewLines(g.board.visual) must_== """
10b 10
. C
C C
"""
          }
        }
      }
    }
  }
}
