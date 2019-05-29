import ornicar.scalalib

package object poker 
    extends scalalib.Validation
    with scalaz.syntax.ToValidationOps {

  type Cards = List[Card]

  type OptionActingRound = AtLeastTwo[Option[Act]]
  type ActingRound = AtLeastTwo[Act]

}
