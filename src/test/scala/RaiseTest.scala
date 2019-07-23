package poker

class RaiseTest extends PokerTest {

  "when raising" should {
    val game = makeGame(10, List(100, 100, 100, 100))

    "on preflop with blinds" should {
      val dealt = game.deal.get

      "dont allow raise less than bb" in {
        dealt.playActs(Raise(9)) must beNone
      }

      "dont allow raise bigger than stack" in {
        dealt.playActs(Raise(90)) must beNone
      }

      "allow min raise" in {
        dealt.playActs(Raise(10)) must beGame("""
10!100b 95s 90B 80!10(. 5 10 20)~!2
R10
""")
      }

      "allow min reraise" in {
        dealt.playActs(Raise(10), Call, Raise(10)) must beGame("""
10!80b 70s 90B 80!10(20 30 10 20)~!0
R10 C R10
""")
      }

      "dont allow raise smaller than largest bet of current round" in {
        dealt.playActs(Raise(25), Raise(24)) must beNone
      }

      "allow raise at least largest bet of current round" in {
        dealt.playActs(Raise(25), Raise(25)) must beGame("""
10!40b 95s 90B 65!25(60 5 10 35)~!3
R25 R25
""")
      }
    }

    "after current betting round ends" in {
      val flop = game.deal
        .get
        .playActs(Raise(25), Call, Call, Call).get

      "next betting round starts with min raise of 1bb" in {
        Some(flop) must beGame("""
10!65b 65s 65B 65!10(35 35 35 35)~!0

C C C R25
""")
      }

      "dont allow less than min raise of 1bb" in {
        flop.playActs(Raise(9)) must beNone
      }

      "allow min raise of 1bb" in {
        flop.playActs(Raise(10)) must beGame("""
10!65b 55s 65B 65!10(35 45 35 35)~!0
R10
C C C R25
""")
      }
    }
  }

  "raise amounts" in {
    val game = makeGame(200, List(10000, 10000, 10000, 10000))

    val postflop = game.deal.get.playActs(Call, Call, Call, Check).get

    "post flop game" in {
      Some(postflop) must beGame("""
200!9800b 9800s 9800B 9800!200(200 200 200 200)~!0

H C C C
""")
    }

    "opens the bet and reraise" in {
      val turnD = postflop.playActs(Raise(600), Raise(1000), Raise(2000))

      turnD must beGame("""
200!9800b 9200s 8200B 6200!2000(200 800 1800 3800)~!2
R2000 R1000 R600
H C C C
""")


      turnD.get.raiseLimits must_== Some(RaiseLimits(2000, 6200))

      val turnA = turnD.get.playActs(Call)

      turnA.get.raiseLimits must_== Some(RaiseLimits(2000, 6200))
    }

  }

}
