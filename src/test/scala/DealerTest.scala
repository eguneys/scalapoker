package poker

class DealerTest extends PokerTest {

  "a table dealer" should {

    val table = makeTable(10)
    
    "when not enough players on table" in {
      table.deal must beFailure
      table.seq(
        _.joinStack(1, 100),
        _.deal) must beFailure
    }

    "when heads up" in {

      "should post blinds" in {
        table.seq(
          _.joinStack(1, 100),
          _.joinStack(2, 100),
          _.deal
        ) must beSuccess.like {
          case table =>
            table.stack(1) must_== 95
            table.stack(2) must_== 90
        }
      }

      "dont allow deal after a deal" in {
        table.seq(
          _.joinStack(1, 100),
          _.joinStack(2, 100),
          _.deal,
          _.deal
        ) must beFailure
      }
    }

  }

}
