package poker

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {
      val game = makeGame(List(100, 100))
      val dealt = game.deal(10).get

      "before deal" in {
        Some(game) must beGame("""
100b 100!(. .)~!
""")
      }

      "after deal" in {
        Some(dealt) must beGame("""
95b 90B!(5 10)~!
""")
      }

      "players check" in {
        "dont allow check small blind" in {
          dealt.playActs(Check) must beNone
        }

        "allow call" in {
          dealt.playActs(Call) must beGame("""
90b 90B!(10 10)~!
C
""")
        }

        "allow call check" in {
          dealt.playActs(Call, Check) must beGame("""
90b 90B!(10 10)~!

H C
""")
        }

        "dont allow call call" in {
          dealt.playActs(Call, Call) must beNone
        }

        "allow call check/check" in {
          dealt.playActs(Call, Check,
            Check) must beGame("""
90b 90B!(10 10)~!
H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check) must beGame("""
90b 90B!(10 10)~!

H H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check,
            Check) must beGame("""
90b 90B!(10 10)~!
H
H H
H C
""")
          dealt.playActs(Call, Check,
            Check, Check,
            Check, Check) must beGame("""
90b 90B!(10 10)~!

H H
H H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check,
            Check, Check,
            Check, Check) must beGame("""
90b 90B!(10 10)~!
H H
H H
H H
H C
""")
        }
      }

      "players raise" in {
        "dont allow raise smaller than big blind" in {
          dealt.playActs(Raise(4)) must beNone
        }

        "dont allow raise bigger than stack" in {
          dealt.playActs(Raise(95)) must beNone
        }

        "allow raise on small blind" in {
          dealt.playActs(Raise(5)) must beGame("""
85b 90B!(15 10)~!
R5
""")
        }

        "dont allow raise smaller than min raise" in {
          dealt.playActs(Raise(5), Raise(4)) must beNone
        }

        "allow raise call" in {
          dealt.playActs(Raise(5), Call) must beGame("""
85b 85B!(15 15)~!

C R5
""")
        }

        "allow reraise" in {
          dealt.playActs(Raise(5), Raise(5)) must beGame("""
85b 80B!(15 20)~!
R5 R5
""")
        }

        "allow reraise call" in {
          dealt.playActs(Raise(5), Raise(5), Call) must beGame("""
80b 80B!(20 20)~!

C R5 R5
""")
        }
      }

      "players fold" in {

        "allow fold" in {
          dealt.playActs(Fold) must beGame("""
95b 90B!(. 10)~!
F
""")
        }

        "dont allow any action after fold" in {
          dealt.playActs(Fold, Check) must beNone
          dealt.playActs(Fold, Call) must beNone
        }
      }
    }
  }
}
