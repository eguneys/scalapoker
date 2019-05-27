package poker

import Rank._

class HandValueTest extends PokerTest {

  "comparing hand values" in {

    "should compare hands" in {
      val hand: Hand = "2s 3s 4h 5c As Ts 8d"
      val hand2: Hand = "5s Ts 3h Ac 2s Ts 8d"
      val hand3: Hand = "5s 5h 3s 3c 2s Ts 3d"

      List(hand, hand2, hand3).sortWith(Hand.sorter) must_== List(hand3, hand, hand2)
    }

    "should compare kickers" in {
      val hand: Hand = "2s 7s 4h 8c As Ts 8d"
      val hand2: Hand = "5s Qs 3h 8c 2s Ts 8d"

      val hand3: Hand = "2s 3s 4h 5c As Ts 8d"
      val hand4: Hand = "5s 4s Qh Ac Ks Ts Jd"

      List(hand2, hand).sortWith(Hand.sorter) must_== List(hand, hand2)

      List(hand3, hand4).sortWith(Hand.sorter) must_== List(hand4, hand3)
    }

    "should compare equal" in {
      val hand: Hand = "2s 3s 4h 5c As Ts 8d"
      val hand2: Hand = "2h 3h 4d 5d Ah Tc 8c"
      val hand3: Hand = "5s Ts 3h Ac 2s Ts 8d"
      
      List(hand, hand2, hand3).sortWith(Hand.sorter) must_== List(hand, hand2, hand3)
    }

  }

  "finding hand values" in {

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
      val hand3: Hand = "5h 3h 3c 5d 2s Ts Td"


      "should find two pair" in {
        hand.value must_== TwoPair(Six, Five, "6s 6h 5c 5d Ts")
        hand2.value must_== TwoPair(Queen, Eight, "Qd Qh 8d 8s Ad")
        hand3.value must_== TwoPair(Ten, Five, "Ts Td 5h 5d 3h")
      }
    }


    "three of a kind" should {
      val hand: Hand = "5c 5s 5h 6c Td 9s 2d"
      val hand2: Hand = "5c 2h Ah Ac 9s As 3d"

      "should find three pair" in {
        hand.value must_== ThreeOfAKind(Five, "5c 5s 5h Td 9s")
        hand2.value must_== ThreeOfAKind(Ace, "Ah Ac As 9s 5c")
      }
    }

    "straight" should {
      val hand: Hand = "5c 6s 3s 2h 5h 4s 5c"
      val hand2: Hand = "5d 6s 7s 8c Ts 9s 2d"
      val hand3: Hand = "5h 6s 6h 7c 2s Ts 8d"
      val hand4: Hand = "5h 6s 6h 7c 3s Ts 8d"
      val hand5: Hand = "5h 6s 6h 7c 3s Ts 8d"
      val hand6: Hand = "1h 2s 4h 3c 6h 7d 5c"

      "should find straight" in {
        hand.value must_== Straight(Six, "6s 5c 4s 3s 2h")
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

    "flush" should {
      val hand: Hand = "4h Th 5h Ac 2h Kh 8d"
      val hand2: Hand = "4s Th 5h Ac 2h Kh 8d"

      "should find flush" in {
        hand.value must_== Flush(King, "Kh Th 5h 4h 2h")
        hand2.value must_== HighCard(Ace, "Ac Kh Th 8d 5h")
      }
    }

    "full house" should {
      val hand: Hand = "Qd Js 3h Qc 7d Jc Jd"
      val hand2: Hand = "9c 9d Jh Jc Js 9h As"

      "should find full house" in {
        hand.value must_== FullHouse(Jack, Queen, "Js Jc Jd Qd Qc")
        hand2.value must_== FullHouse(Jack, Nine, "Jh Jc Js 9c 9d")
      }

      "should not find full house" in {
        val hand: Hand = "5h 3h 3c 5d 2s Ts Td"
        val hand2: Hand = "5s 9d Kd 6h 7s 7d Kh"
        val hand3: Hand = "9h 9s 3d 5c Kd 5d Kh"

        hand.value must_== TwoPair(Ten, Five, "Ts Td 5h 5d 3h")
        hand2.value must_== TwoPair(King, Seven, "Kd Kh 7s 7d 9d")

        hand3.value must_== TwoPair(King, Nine, "Kd Kh 9h 9s 5c")
      }

      "should pick high kickers" in {
        val hand: Hand = "5d 5h 3h 3c Qh Qd Qs"
        val hand2: Hand = "9c Qs 9h 5h Ts Qc Qh"

        hand.value must_== FullHouse(Queen, Five, "Qh Qd Qs 5d 5h")
        hand2.value must_== FullHouse(Queen, Nine, "Qs Qc Qh 9c 9h")

      }
    }

    "four of a kind" should {
      val hand: Hand = "7h 7d 3s 2c 7s 7c 4s"
      val hand2: Hand = "7h 3d 3s 2c 7s 7c 4s"

      "should find four of a kind" in {
        hand.value must_== FourOfAKind(Seven, "7h 7d 7s 7c 4s")
        hand2.value must_== FullHouse(Seven, Three, "7h 7s 7c 3d 3s")
      }
    }

    "straight flush" should {
      val hand: Hand = "Qd 7s 8s Js Kh Ts 9s"
      val hand2: Hand = "Qd 7s 8s Js Kh Ts 9d"
      val hand3: Hand = "Ad 2d 3d 4d 5d Ts 9d"

      "should find straight flush" in {
        hand.value must_== StraightFlush(Jack, "Js Ts 9s 8s 7s")
        hand2.value must_== Straight(King, "Kh Qd Js Ts 9d")
        hand3.value must_== StraightFlush(Five, "5d 4d 3d 2d ad")
      }
    }
  }
}
