package poker

class TableTest extends PokerTest {

  "a table" should {

    val table = makeTable(10)

    "be empty" in {
      table.board must beNone
    }

    "allow player join with ten big blinds" in {
      table.joinStack(1, 100) must beSome.like {
        case table =>
          table.board must beNone
      }
      table.joinStack(1, 100) must beSome
      table.joinStack(9, 100) must beSome
    }

    "dont allow low stack to join" in {
      table.joinStack(1, 99) must beNone
    }

    "dont allow same seat join" in {
      table.seq(
        _.joinStack(1, 100),
        _.joinStack(1, 100)
      ) must beNone
    }

    "dont allow bad index to join" in {
      table.joinStack(0, 100) must beNone
      table.joinStack(10, 100) must beNone
    }

    "allow second player join" in {
      table.seq(
        _.joinStack(1, 100),
        _.joinStack(2, 100)) must beSome.like {
        case table =>
          table.board must beSome
      }
    }

  }

}
