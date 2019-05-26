package poker
package format

// "Kh Tc 5d As 3c 3s 2h"
object Visual {

  def >>(hand: Hand): String = hand.cards.map(_.forsyth) mkString " "

  def <<(visual: String): Hand = {
    Hand(list(visual))
  }

  def list(visual: String): List[Card] = {
    val l = visual split " "
    for {
      card <- l.toList
      rank <- Rank forsyth card.charAt(0)
      suit <- Suit forsyth card.charAt(1)
    } yield Card(rank, suit)
  }

}
