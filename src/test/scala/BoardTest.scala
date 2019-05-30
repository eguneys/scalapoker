package poker

class BoardTest extends PokerTest {

  "a board" should {

    val twoPlayer = makeBoard(List.fill(2)(10))
    val threePlayer = makeBoard(List.fill(3)(10))
    val fourPlayer = makeBoard(List.fill(4)(10))

    val twoPlayer2 = twoPlayer.deal(1).get
    val threePlayer2 = threePlayer.deal(1).get
    val fourPlayer2 = fourPlayer.deal(1).get

    "decide small blind and big blind" in {

      // button is always small blind in heads up
      twoPlayer.button must_== 0
      twoPlayer.smallBlind must_== 0
      twoPlayer.bigBlind must_== 1

      threePlayer.button must_== 0
      threePlayer.smallBlind must_== 1
      threePlayer.bigBlind must_== 2

      fourPlayer.button must_== 0
      fourPlayer.smallBlind must_== 1
      fourPlayer.bigBlind must_== 2
    }

    "decide first to acts" in {
      twoPlayer.firstToAct must_== 0
      threePlayer.firstToAct must_== 0
      fourPlayer.firstToAct must_== 3

      twoPlayer.firstToActOnFlop must_== 1
      threePlayer.firstToActOnFlop must_== 1
      fourPlayer.firstToActOnFlop must_== 1

      twoPlayer.toAct must_== None

      twoPlayer2.toAct must_== Some(0)
      threePlayer2.toAct must_== Some(0)
      fourPlayer2.toAct must_== Some(3)
    }

    "dont allow a check before a deal" in {
      twoPlayer.check must beNone
    }

    "allow a check on first act" in {

      twoPlayer2.check must beSome.like {
        case b =>
          b.toAct must_== Some(1)
      }
      threePlayer2.check must beSome.like {
        case b => 
          b.toAct must_== Some(1)
      }
      fourPlayer2.check must beSome.like {
        case b => 
          b.toAct must_== Some(0)
      }
    }

    "allow a check on second act" in {
      twoPlayer2.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b =>
          b.toAct must beNone
      }
      threePlayer2.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must_== Some(2)
      }
      fourPlayer2.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must_== Some(1)
      }
    }

    "should find toAct" in {
      twoPlayer2.seq(
        _ check,
        _ check
      ) must beSome.like {
        case b =>
          b.toAct must beNone
      }
      threePlayer2.seq(
        _ check,
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must beNone
      }
      fourPlayer2.seq(
        _ check,
        _ check,
        _ check,
        _ check
      ) must beSome.like {
        case b => 
          b.toAct must beNone
      }
    }

    "should find first to act" in {
      "preflop" in {
        twoPlayer.firstToAct must_== 0
        threePlayer.firstToAct must_== 0
      }
      "post flop" in {
        twoPlayer2.seq(
          _ check,
          _ check,
          _ nextRound
        ) must beSome.like {
          case b =>
            b.firstToAct must_== 1
        }
        threePlayer2.seq(
          _ check,
          _ check,
          _ check,
          _ nextRound
        ) must beSome.like {
          case b =>
            b.firstToAct must_== 1
        }
      }
    }

    "dont allow check second time" in {
      twoPlayer2.seq(
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
      twoPlayer2.seq(
        _ check,
        _ check,
        _ nextRound) must beSome.like {
        case b =>
          b.roundActs must_== Board.emptyRoundActs(twoPlayer.stacks)
          b.history must_== History(true,
            ActingRounds(List(AtLeastTwo(Check, Check))))
      }
    }

  }

}
