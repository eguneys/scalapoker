package poker

import Rank._

class ScoreTest extends PokerTest {

  "a hand" should {
    val hand1: Hand = "Kh Tc 5d As 3c 3s 2h"
    val hand2: Hand = "8d 8s Qd Ad Qh Tc 9c"
    val hand3: Hand = "4s 4h Ah Jc Ts 7s 8d"
    val hand4: Hand = "5h 5c 7s 6c Ts 9s 2d"
    val hand5: Hand = "5h 6s Jh 7c 2s Ts 8d"

    "should find hand value" in {
      hand1.value must_== OnePair(Three, "As Kh Tc 5d 3c 3s 2h")
      hand2.value must_== TwoPair(Eight, Queen, "Qd Qh 8d 8s Ad Tc 9c")
      hand3.value must_== OnePair(Four, "4s 4h Ah Jc Ts 8d 7s")
      hand4.value must_== OnePair(Five, "5h 5c Ts 9s 7s 6c 2d")
      hand5.value must_== HighCard(Jack, "Jh Ts 8d 7c 6s 5h 2s")
    }
  }

  "two pair" should {
    val hand: Hand = "5c 6s 6h 7c 5d Ts 8d"

    "should find hand value" in {
      hand.value must_== TwoPair(Six, Five, "6s 6h 5c 5d Ts 8d 7c")
    }
  }



}
