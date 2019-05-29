package poker

class MoveTest extends PokerTest {

  "playing a game" should {

    "check" in {
      val game = makeGame(List.fill(2)(10))

      "dont allow on first move" in {
        game must bePoss(Call, Fold)
      }
    }
  }
}
