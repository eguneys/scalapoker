import ornicar.scalalib

package object poker 
    extends scalalib.Validation
    with scalaz.syntax.ToValidationOps {

  type Cards = List[Card]

  type ActingRound = List[Act]

  type SeatIndex = Int
  
  type StackIndex = Int

}
