package poker

class BoardTest extends PokerTest {

  "a board" should {

    val twoPlayer = makeBoard(List.fill(2)(10))
    val threePlayer = makeBoard(List.fill(3)(10))
    val fourPlayer = makeBoard(List.fill(4)(10))

    "decide small blind and big blind" in {

      twoPlayer.button must_== 0
      twoPlayer.smallBlind must_== 1
      twoPlayer.bigBlind must_== 0

      threePlayer.button must_== 0
      threePlayer.smallBlind must_== 1
      threePlayer.bigBlind must_== 2

      fourPlayer.button must_== 0
      fourPlayer.smallBlind must_== 1
      fourPlayer.bigBlind must_== 2
    }

    "decide first to acts" in {
      twoPlayer.firstToAct must_== 1
      threePlayer.firstToAct must_== 0
      fourPlayer.firstToAct must_== 3

      twoPlayer.firstToActOnFlop must_== 1
      threePlayer.firstToActOnFlop must_== 1
      fourPlayer.firstToActOnFlop must_== 1

      twoPlayer.toAct must_== Some(1)
      threePlayer.toAct must_== Some(0)
      fourPlayer.toAct must_== Some(3)
    }

    "allow a check on first act" in {
      twoPlayer.check must beSome.like {
        case b =>
          b.toAct must_== Some(0)
      }
      threePlayer.check must beSome.like {
        case b => 
          b.toAct must_== Some(1)
      }
      fourPlayer.check must beSome.like {
        case b => 
          b.toAct must_== Some(0)
      }
    }

    "allow a check on second act" in {
      twoPlayer.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b =>
          b.toAct must beNone
      }
      threePlayer.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must_== Some(2)
      }
      fourPlayer.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must_== Some(1)
      }
    }

    "should find toAct" in {
      twoPlayer.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b =>
          b.toAct must beNone
      }
      threePlayer.seq(
        _ check,
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must beNone
      }
      fourPlayer.seq(
        _ check,
        _ check,
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must beNone
      }
    }

    "dont allow check second time" in {
      twoPlayer.seq(
        _ check,
        _ check,
        _ check
      ) must beNone
    }

    "dont allow next round" in {
      twoPlayer.nextRound must beNone
      threePlayer.nextRound must beNone
      fourPlayer.nextRound must beNone
    }

    "allow next round" in {
      twoPlayer.seq(
        _ check,
        _ check,
        _ nextRound) must beSome.like {
        case b =>
          b.roundActs must_== AtLeastTwo(None, None)
          b.history must_== List(AtLeastTwo(Check, Check))
      }
    }

  }

}
