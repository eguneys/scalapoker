package poker

import Rank._

class ScoreTest extends PokerTest {
  "high card" should {
    val hand: Hand = "5h 6s Jh 7c 2s Ts 8d"

    "should find high card" in {
      hand.value must_== HighCard(Jack, "Jh Ts 8d 7c 6s 5h 2s")
    }
  }


  "one pair" should {
    val hand1: Hand = "Kh Tc 5d As 3c 3s 2h"
    val hand2: Hand = "4s 4h Ah Jc Ts 7s 8d"
    val hand3: Hand = "5h 5c 7s 6c Ts 9s 2d"


    "should find one pair" in {
      hand1.value must_== OnePair(Three, "3c 3s As Kh Tc 5d 2h")
      hand2.value must_== OnePair(Four, "4s 4h Ah Jc Ts 8d 7s")
      hand3.value must_== OnePair(Five, "5h 5c Ts 9s 7s 6c 2d")
    }
  }

  "two pair" should {
    val hand: Hand = "5c 6s 6h 7c 5d Ts 8d"
    val hand2: Hand = "8d 8s Qd Ad Qh Tc 9c"

    "should find two pair" in {
      hand.value must_== TwoPair(Six, Five, "6s 6h 5c 5d Ts 8d 7c")
      hand2.value must_== TwoPair(Queen, Eight, "Qd Qh 8d 8s Ad Tc 9c")
    }
  }



}
