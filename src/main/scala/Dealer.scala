package poker

case class Dealer() {

  // player i 2
  // flop 3
  // turn 1
  // river 1
  lazy val shuffled = scala.util.Random.shuffle(Dealer.deck)

  def flop = shuffled.take(3)

  def turn = shuffled.drop(3).head

  def river =
    shuffled.drop(3 + 1).head

  def player(i: Int) =
    shuffled.drop(3 + 3 + i * 2).take(2)

  def middle = flop :+ turn :+ river

  def hand(i: Int) = Hand(player(i) ++ middle)


  def hands(players: Int) = List.range(0, players) map hand
}

object Dealer {

  import Rank._

  val deck = List(
    Ace of Diamonds,
    Two of Diamonds,
    Three of Diamonds,
    Four of Diamonds,
    Five of Diamonds,
    Six of Diamonds,
    Seven of Diamonds,
    Eight of Diamonds,
    Nine of Diamonds,
    Ten of Diamonds,
    Jack of Diamonds,
    Queen of Diamonds,
    King of Diamonds,

    Ace of Hearts,
    Two of Hearts,
    Three of Hearts,
    Four of Hearts,
    Five of Hearts,
    Six of Hearts,
    Seven of Hearts,
    Eight of Hearts,
    Nine of Hearts,
    Ten of Hearts,
    Jack of Hearts,
    Queen of Hearts,
    King of Hearts,

    Ace of Spades,
    Two of Spades,
    Three of Spades,
    Four of Spades,
    Five of Spades,
    Six of Spades,
    Seven of Spades,
    Eight of Spades,
    Nine of Spades,
    Ten of Spades,
    Jack of Spades,
    Queen of Spades,
    King of Spades,

    Ace of Clubs,
    Two of Clubs,
    Three of Clubs,
    Four of Clubs,
    Five of Clubs,
    Six of Clubs,
    Seven of Clubs,
    Eight of Clubs,
    Nine of Clubs,
    Ten of Clubs,
    Jack of Clubs,
    Queen of Clubs,
    King of Clubs
  )

}
