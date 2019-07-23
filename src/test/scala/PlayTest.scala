package poker

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {
      val game = makeGame(10, List(100, 100))
      val dealt = game.deal.get

      "before deal" in {
        Some(game) must beGame("""
10!100b 100!0(. .)~!
""")
      }

      "after deal" in {
        Some(dealt) must beGame("""
10!95b 90B!10(5 10)~!1
""")
      }

      "players check" in {
        "dont allow check small blind" in {
          dealt.playActs(Check) must beNone
        }

        "allow call" in {
          dealt.playActs(Call) must beGame("""
10!90b 90B!10(10 10)~!1
C
""")
        }

        "allow call check" in {
          dealt.playActs(Call, Check) must beGame("""
10!90b 90B!10(10 10)~!0

H C
""")
        }

        "dont allow call call" in {
          dealt.playActs(Call, Call) must beNone
        }

        "allow call check/check" in {
          dealt.playActs(Call, Check, Check) must beGame("""
10!90b 90B!10(10 10)~!0
H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check) must beGame("""
10!90b 90B!10(10 10)~!0

H H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check,
            Check) must beGame("""
10!90b 90B!10(10 10)~!0
H
H H
H C
""")
          dealt.playActs(Call, Check,
            Check, Check,
            Check, Check) must beGame("""
10!90b 90B!10(10 10)~!0

H H
H H
H C
""")

          dealt.playActs(Call, Check,
            Check, Check,
            Check, Check,
            Check, Check) must beGame("""
10!90b 90B!10(10 10)~!
H H
H H
H H
H C
""")
        }
      }

      "players raise" in {
        "dont allow raise smaller than big blind" in {
          dealt.playActs(Raise(9)) must beNone
        }

        "dont allow raise bigger than stack" in {
          dealt.playActs(Raise(95)) must beNone
        }

        "allow raise on small blind" in {
          dealt.playActs(Raise(10)) must beGame("""
10!80b 90B!10(20 10)~!1
R10
""")
        }

        "dont allow raise smaller than min raise" in {
          dealt.playActs(Raise(20), Raise(19)) must beNone
        }

        "allow raise call" in {
          dealt.playActs(Raise(10), Call) must beGame("""
10!80b 80B!10(20 20)~!0

C R10
""")
        }

        "allow reraise" in {
          dealt.playActs(Raise(20), Raise(20)) must beGame("""
10!70b 50B!20(30 50)~!0
R20 R20
""")
        }

        "allow reraise call" in {
          dealt.playActs(Raise(10), Raise(10), Call) must beGame("""
10!70b 70B!10(30 30)~!0

C R10 R10
""")
        }
      }

      "players fold" in {

        "allow fold" in {
          dealt.playActs(Fold) must beGame("""
10!95b 90B!10(5 10)~!1
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
