package poker

class BoardTest extends PokerTest {

  "a board" should {

    val twoPlayer = makeBoard(10, List.fill(2)(10))
    val threePlayer = makeBoard(10, List.fill(3)(10))
    val fourPlayer = makeBoard(10, List.fill(4)(10))

    val twoPlayerGame = makeGame(10, List(100, 100)).deal.get
    val threePlayerGame = makeGame(10, List(100, 100, 100)).deal.get
    val fourPlayerGame = makeGame(10, List(100, 100, 100, 100)).deal.get

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

      "after fold" in {

        twoPlayerGame.playActs(Fold) must beSome.like {
          case b =>
            b.board.toAct must beNone
        }

        twoPlayerGame.playActs(Fold) must beGame("""
10!95b 90B!10(5 10)~!1
F
""")

        // b s B .
        // C R F C
        fourPlayerGame.playActs(Call, Call, Raise(10), Fold) must beSome.like {
          case b =>
            b.board.toAct must_== Some(3)
        }

        // b s B .
        // CF R F CR
        fourPlayerGame.playActs(Call, Call, Raise(10), Fold, Raise(10), Fold) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }

      "after allin" in {

        twoPlayerGame.playActs(AllIn) must beSome.like {
          case b =>
            b.board.toAct must beNone
        }

        // b s B .
        // C R F C
        fourPlayerGame.playActs(Call, Call, Raise(10), AllIn) must beSome.like {
          case b =>
            b.board.toAct must_== Some(3)
        }

        // b s B .
        // CF R F CR
        fourPlayerGame.playActs(Call, Call, Raise(10), Fold, Raise(10), AllIn) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }

      "after allin skip" in {

        // b s B .
        // CA RA A CA
        fourPlayerGame.playActs(Call, Call, Raise(10), AllIn, AllIn, AllIn) must beSome.like {
          case b =>
            b.board.toAct must beNone
        }
      }
    }

    "find to act after flop" in {
      "after raise 2" in {

        // b s B .
        // CC CC R C
        fourPlayerGame.playActs(Call, Call, Call, Raise(10), Call, Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }

        // b s B
        // CC CC R
        threePlayerGame.playActs(Call, Call, Raise(10), Call, Call) must beSome.like {
          case b =>
            b.board.toAct must_== Some(1)
        }
      }
    }

    "after all in not a full raise" should {

      val game = makeGame(200, List(1500, 1000, 750)).deal.get

      val game2 = game.playActs(Raise(300), Call, AllIn)


      "find to act" in {
        game2 must beGame("""
200!1000b 500s 0B!300(500 500 750)~!
A C R300
""")

        game2 must beSome.like {
          case g =>
            g.board.toAct must_== Some(0)
            g.board.actingRounds.isPreflop must_== true
        }
      }

      "allow call" in {
        game2.get.playActs(Call, Call) must beGame("""
200!750b 250s 0B!200(750 750 750)~!0

C C A C R300
""")
      }

      "full raise rule dont allow reraise" in {
        game2 must bePoss(Call, Fold)

        game2.get.playActs(Raise(500)) must beNone
      }

      "full raise rule dont allow second allin" in {
        val game = makeGame(200, List(7500, 810, 820)).deal.get

        val game2 = game.playActs(
          Raise(300), Call, Raise(300),
          Call, AllIn)

        game2 must bePoss(Call, Fold)
      }

      "full raise rule dont allow rereaise extra call" in {
        val game = makeGame(200, List(7500, 810, 7500)).deal.get

        val game2 = game.playActs(
          Raise(300), Call, Raise(300),
          Call, AllIn)

        game2 must beGame("""
200!6700b 0s 6700B!300(800 810 800)~!
A C R300 C R300
""")

        game2 must bePoss(Call, Fold)

        game2 must beSome.like {
          case g =>
            g.board.toAct must_== Some(2)
        }
      }

      "full raise rule allow reraise next round" in {
        val game = makeGame(200, List(7500, 800, 7500)).deal.get

        val game2 = game.playActs(
          Raise(300), Call, Raise(300),
          Call, AllIn)

        game2 must beGame("""
200!6700b 0s 6700B!200(800 800 800)~!0

A C R300 C R300
""")

//         game2 must bePoss(Check, Fold)

//         game2.get.playActs(Check, Check) must beGame("""
// 6700b 0s 6700B!300(800 800 800)~5!0

// H H A C R300 C R300
// """)
      }
    }
  }
} 
