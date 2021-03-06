package poker
package format

import Rank._

class VisualTest extends PokerTest {

  val f = Visual
  val h = HandVisual

  "The visual hand formatter" should {
    "export hand" in {
      val hand = Hand(List(King of Hearts, Ten of Clubs, Five of Diamonds, Ace of Spades, Three of Clubs, Three of Spades, Two of Hearts))

      h >> hand must_== "Kh Tc 5d As 3c 3s 2h"
      h << (h >> hand) must_== hand
    }
  }

  "The visual board formatter" should {
    "export board" in {
      f.addNewLines(f >> (makeBoard(10, List.fill(2)(10)))) must_== newBoardFormat
    }

    "import and export is non destructive" in {
      forall(examples) { example =>
        f.addNewLines(f >> (f << example)) must_== example
      }
    }
  }

  val newBoardFormat = """
10!10b 10!0(. .)~!
"""

  val examples = Seq(
    newBoardFormat, """
10!10 10b 10!10(. . .)~!

H H
""", """
10!10b 10 10!10(. . .)~!

H H
""", """
10!10b 10 10!10(. . .)~!

H H
H H H
H H H
""", """
90!10 10b 10!10(. . .)~!
H H H
""", """
90!10 10b 10!100(. . .)~!
H H H R10 R10 R10 R10 R10
""", """
100!10b 10 10!20(. . .)~!
C R1 R10 C C
""", """
100!10b 10 10!10(. . .)~!
H H
H H H R10 R10 R10 C C C H H H H H H H H R10
H H H H H H C C R10 H H H H H
""", """
100!10 10b 10!10(. . .)~!
C C C
"""
  )

}
