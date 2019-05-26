package poker
package format

import Rank._

class VisualTest extends PokerTest {

  val f = Visual

  "The visual hand formatter" should {
    "export hand" in {
      val hand = Hand(List(King of Hearts, Ten of Clubs, Five of Diamonds, Ace of Spades, Three of Clubs, Three of Spades, Two of Hearts))

      f >> hand must_== "Kh Tc 5d As 3c 3s 2h"
      f << (f >> hand) must_== hand
    }
  }

}
