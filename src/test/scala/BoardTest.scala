package poker

class BoardTest extends PokerTest {

  "a board" should {

    val twoPlayer = makeBoard(List.fill(2)(10))
    val threePlayer = makeBoard(List.fill(3)(10))
    val fourPlayer = makeBoard(List.fill(4)(10))

    val twoPlayerGame = makeGame(List(100, 100)).deal(10).get
    val threePlayerGame = makeGame(List(100, 100, 100)).deal(10).get
    val fourPlayerGame = makeGame(List(100, 100, 100, 100)).deal(10).get

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

    "should find first to acts" in {
      twoPlayer.firstToAct must_== 0
      threePlayer.firstToAct must_== 0
      fourPlayer.firstToAct must_== 3

      twoPlayer.firstToActOnFlop must_== 1
      threePlayer.firstToActOnFlop must_== 1
      fourPlayer.firstToActOnFlop must_== 1

      twoPlayer.toAct must_== None

      twoPlayerGame.board.toAct must_== Some(0)
      threePlayerGame.board.toAct must_== Some(0)
      fourPlayerGame.board.toAct must_== Some(3)
    }

    "should find first to act" in {
      "preflop" in {
        twoPlayer.firstToAct must_== 0
        threePlayer.firstToAct must_== 0
      }
      "post flop" in {
        twoPlayerGame.playActs(Call, Check) must beSome.like {
          case b =>
            b.board.firstToAct must_== 1
        }
        threePlayerGame.playActs(Call, Call, Check) must beSome.like {
          case b =>
            b.board.firstToAct must_== 1
        }
      }
    }

    "find to act" should {

      "find toAct after a call" in {

        twoPlayerGame.playActs(Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
        threePlayerGame.playActs(Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
        fourPlayerGame.playActs(Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(0)
        }
      }

      "find toAct after two calls" in {
        twoPlayerGame.playActs(Call, Check) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
        threePlayerGame.playActs(Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(2)
        }
        fourPlayerGame.playActs(Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }

      "find to act after preflop" in {
        twoPlayerGame.playActs(Call, Check, Check) must beSome.like {
          case b =>
            b.board.toAct must_== Some(0)
        }
        threePlayerGame.playActs(Call, Call, Check) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
        fourPlayerGame.playActs(Call, Call, Call, Check) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }

      "after raise" in {
        fourPlayerGame.playActs(Call, Call, Raise(10), Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(0)
        }
        threePlayerGame.playActs(Call, Call, Raise(10), Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }

    }
  }
} 
