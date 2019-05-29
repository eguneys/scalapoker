package poker

class TableTest extends PokerTest {

  "a table" should {

    val table = makeTable(10)

    "be empty" in {
      table.game must beNone
    }

    "allow player join with ten big blinds" in {
      table.joinStack(1, 100) must beSuccess.like {
        case table =>
          table.game must beNone
          table.nbPlayers must_== 1
      }
      table.joinStack(1, 100) must beSuccess
      table.joinStack(9, 100) must beSuccess
    }

    "dont allow low stack to join" in {
      table.joinStack(1, 99) must beFailure
    }

    "dont allow same seat join" in {
      table.seq(
        _.joinStack(1, 100),
        _.joinStack(1, 100)
      ) must beFailure
    }

    "dont allow bad index to join" in {
      table.joinStack(0, 100) must beFailure
      table.joinStack(10, 100) must beFailure
    }

    "allow second player join" in {
      table.seq(
        _.joinStack(1, 100),
        _.joinStack(2, 100)) must beSuccess.like {
        case table =>
          table.nbPlayers must_== 2
          table.game must beSome
      }
    }

    "leave table" in {

      "allow leave after join" in {
        table.joinStack(1, 100) must beSuccess.like {
          case oneplayer =>
            oneplayer.leaveStack(1)  must beSome.like {
              case (table, leaveStack) =>
                table.game must beNone
                table.nbPlayers must_== 0
                leaveStack must_== 100
            }
        }
      }

      "dont allow empty seat leave" in {
        table.leaveStack(1)  must beNone
      }

      "dont allow bad index to leave" in {
        table.leaveStack(0) must beNone
        table.leaveStack(10) must beNone
      }

      "should return remaining stack before a round" in {
        table.seq(
          _.joinStack(1, 100),
          _.joinStack(2, 100)
        ) must beSuccess.like {
          case twoplayer =>
            twoplayer.leaveStack(1)  must beSome.like {
              case (table, leaveStack) =>
                table.nbPlayers must_== 1
                leaveStack must_== 100
            }
        }
      }

      "should return remaining stack after a round" in {
        table.seq(
          _.joinStack(1, 100),
          _.joinStack(2, 100),
          _.deal
        ) must beSuccess.like {
          case twoplayer =>
            twoplayer.leaveStack(1)  must beSome.like {
              case (table, leaveStack) =>
                table.nbPlayers must_== 1
                leaveStack must_== 95
            }
        }
      }
    }

  }

}
