package object poker {

  type Cards = List[Card]

  type OptionActingRound = AtLeastTwo[Option[Act]]
  type ActingRound = AtLeastTwo[Act]

}
