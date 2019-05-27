package poker

import Rank._

class ScoreTest extends PokerTest {
  "high card" should {
    val hand: Hand = "5h 6s Jh 7c 2s Ts 8d"

    "should find high card" in {
      hand.value must_== HighCard(Jack, "Jh Ts 8d 7c 6s")
    }
  }


  "one pair" should {
    val hand1: Hand = "Kh Tc 5d As 3c 3s 2h"
    val hand2: Hand = "4s 4h Ah Jc Ts 7s 8d"
    val hand3: Hand = "5h 5c 7s 6c Ts 9s 2d"


    "should find one pair" in {
      hand1.value must_== OnePair(Three, "3c 3s As Kh Tc")
      hand2.value must_== OnePair(Four, "4s 4h Ah Jc Ts")
      hand3.value must_== OnePair(Five, "5h 5c Ts 9s 7s")
    }
  }

  "two pair" should {
    val hand: Hand = "5c 6s 6h 7c 5d Ts 8d"
    val hand2: Hand = "8d 8s Qd Ad Qh Tc 9c"

    "should find two pair" in {
      hand.value must_== TwoPair(Six, Five, "6s 6h 5c 5d Ts")
      hand2.value must_== TwoPair(Queen, Eight, "Qd Qh 8d 8s Ad")
    }
  }


  "three of a kind" should {
    val hand: Hand = "5c 5s 5h 6c Td 9s 2d"
    val hand2: Hand = "5c 2h Ah Ac 9s As 2d"

    "should find three pair" in {
      hand.value must_== ThreeOfAKind(Five, "5c 5s 5h Td 9s")
      hand2.value must_== ThreeOfAKind(Ace, "Ah Ac As 9s 5c")
    }
  }

  "straight" should {
    val hand: Hand = "5c 6s 3s 2s 5s 4s 5c"
    val hand2: Hand = "5d 6s 7s 8c Ts 9s 2d"
    val hand3: Hand = "5h 6s 6h 7c 2s Ts 8d"
    val hand4: Hand = "5h 6s 6h 7c 3s Ts 8d"
    val hand5: Hand = "5h 6s 6h 7c 3s Ts 8d"
    val hand6: Hand = "1h 2s 4h 3c 6h 7d 5c"

    "should find straight" in {
      hand.value must_== Straight(Six, "6s 5c 4s 3s 2s")
      hand2.value must_== Straight(Ten, "Ts 9s 8c 7s 6s")
      hand3.value must_== OnePair(Six, "6s 6h Ts 8d 7c")
      hand4.value must_== OnePair(Six, "6s 6h Ts 8d 7c")
      hand6.value must_== Straight(Seven, "7d 6h 5c 4h 3c")
    }

    "should find mixed straight" in {
      val hand: Hand = "9s 3s 4h 5c 6s 2s 9d"
      hand.value must_== Straight(Six, "6s 5c 4h 3s 2s")
    }

    "should find inner straight" in {
      val hand6: Hand = "4h 5s 6s 8d 9s Ts 7c"
      hand6.value must_== Straight(Ten, "Ts 9s 8d 7c 6s")
      
    }

    "should detect a low ace" in {
      val hand: Hand = "2c 3s 4h 5c As Ts 8d"

      hand.value must_== Straight(Five, "5c 4h 3s 2c as")
    }

    "should detect a high ace" in {
      val hand: Hand = "2d 3s Jh Qc As Ts Kd"
      val hand2: Hand = "2d 3s 4h 7c As Ts Kd"

      hand.value must_== Straight(Ace, "As Kd Qc Jh Ts")
      hand2.value must_== HighCard(Ace, "As Kd Ts 7c 4h")
    }

    "should know ace is not high in a wheel" in {
      val lowHand: Hand = "2s 3s 4h 5c As Ts 8d"
      val highHand: Hand = "2s 3s 4h 5c 6s Ts 8d"

      lowHand.value must_== Straight(Five, "5c 4h 3s 2s as")
      highHand.value must_== Straight(Six, "6s 5c 4h 3s 2s")
    }
  }
}
