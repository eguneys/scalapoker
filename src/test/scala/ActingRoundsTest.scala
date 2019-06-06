package poker

import format.Visual.addNewLines

class ActingRoundsTest extends PokerTest {

  "playing a game" should {

    "heads up" in {
      val game = makeGame(List(100, 100))
      val dealt = game.deal(10).get

      "before post blinds" in {
        addNewLines(game.board.visual) must_== """
100b 100!0(. .)~!
"""
      }

      "players check" in {

        "call" in {
          dealt.playActs(Call) must beGame("""
90b 90B!10(10 10)~!1
C
""")
        }

        "call check" in {
          dealt.playActs(Call, Check) must beGame("""
90b 90B!10(10 10)~!0

H C
""")
        }

        "call check check" in {
          dealt.playActs(Call, Check, Check) must beGame("""
90b 90B!10(10 10)~!0
H
H C
""")
        }
      }

      "players fold" in {
        dealt.playActs(Call, Fold) must beGame("""
90b 90B!10(10 10)~!
F C
""")
      }
    }
  }
}
