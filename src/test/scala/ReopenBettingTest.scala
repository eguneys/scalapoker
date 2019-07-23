package poker

class ReopenBettingTest extends PokerTest {

  "when raising" should {
    "Example 1." in {
      val game = makeGame(100, List(1000, 1000, 225, 1000, 300))

      val postflop = game.deal
        .get
        .playActs(Call, Call, Call, Call, Check)

      "postflop" in {
        postflop must beGame("""
100!900b 900s 125B 900 200!100(100 100 100 100 100)~!0

H C C C C
""")
      }

      "Series of short all-in wagers that add up to a full raise re-open betting" in {

        val s1 = postflop.get.playActs(Raise(100), AllIn, Call, AllIn, Call)

        "Example 1-A" in {
          s1 must beGame("""
100!700b 800s 0B 775 0!100(300 200 225 225 300)~!
C A C A R100
H C C C C
""")

          s1 must bePoss(Call, Fold, AllIn)
          s1.get.playActs(Raise(100)) must beSome
          s1.get.raiseLimits must_== Some(RaiseLimits(100, 700))

          val s2 = s1.get.playActs(Call)

          s2 must bePoss(Call, Fold)

          s2.get.playActs(Raise(100)) must beNone

          s2.get.raiseLimits must beNone
        }

        "Example 1-B" in {
          val s2 = s1.get.playActs(Raise(100))
          s2 must bePoss(Call, Fold, AllIn)
          s2.get.playActs(Raise(100)) must beSome
          s2.get.raiseLimits must_== Some(RaiseLimits(100, 600))
        }
      }
    }

    "Example 2." in {
      val game = makeGame(4000, List(7500, 40000, 40000, 40000, 40000))

      val preflop = game.deal
        .get
        .playActs(Call, Fold, AllIn, Fold)

      "preflop" in {
        preflop must beGame("""
4000!0b 38000s 36000B 36000 40000!4000(7500 2000 4000 4000 .)~!2
F A F C
""")
      }

      "Example 2-A" in {

        preflop must bePoss(Fold, Call, AllIn)
        preflop must bePossRaise(Raise(4000))
        preflop.get.raiseLimits must_== Some(RaiseLimits(4000, 32500))

        val s2 = preflop.get.playActs(Call)

        s2 must bePoss(Fold, Call)
        s2.get.playActs(Raise(4000)) must beNone
        s2.get.raiseLimits must beNone
      }

      "Example 2-B" in {

        val s2 = preflop.get.playActs(Raise(4000))
        s2 must bePoss(Fold, Call, AllIn)
        s2 must bePossRaise(Raise(4000))
        s2.get.raiseLimits must_== Some(RaiseLimits(4000, 28500))
      }
    }

    "Example SO" in {
      val game = makeGame(200, List(1000, 1000, 750))

      val s1 = game.deal.get.playActs(Raise(300))

      s1 must beGame("""
200!500b 900s 550B!300(500 100 200)~!2
R300
""")

      val s2 = s1.get.playActs(Call, AllIn)

      s2 must bePoss(Call, Fold)
      s2.get.playActs(Raise(300)) must beNone
      s2.get.raiseLimits must beNone
    }
  }
}
