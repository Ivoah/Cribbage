case class Hand(cards: Set[Card]) {
  def score(cut: Card, crib: Boolean = false): Map[String, Map[Set[Card], Int]] = {
    Map(
      "Fifteen" -> fifteens(cards + cut),
      "Pair" -> pairs(cards + cut),
      "Run" -> run(cards + cut),
      "Flush" -> (if (crib) flush(cards + cut) else flush(cards, Some(cut))),
      "Right jack" -> jack(cards, cut)
    )
  }

  private def fifteens(cards: Set[Card]): Map[Set[Card], Int] =
    cards.subsets.filter(_.toSeq.map(_.scoreValue).sum == 15).map(_ -> 2).toMap

  private def pairs(cards: Set[Card]): Map[Set[Card], Int] =
    cards.subsets(2).filter(_.map(_.value).size == 1).map(_ -> 2).toMap

  private def run(cards: Set[Card]): Map[Set[Card], Int] = {
    val runs = (3 to cards.size).flatMap(n => cards.subsets(n).map(_.toSeq.sorted).filter(_.sliding(n).exists(_.isRun())))
    val uniqueRuns = runs.filterNot(s => runs.exists(l => s.length < l.length && s.forall(l.contains)))
    uniqueRuns.map(r => r.toSet -> r.length).toMap
  }

  private def flush(cards: Set[Card], cut: Option[Card] = None): Map[Set[Card], Int] =
    val suits = cards.map(_.suit)
    if (suits.size == 1) {
      if (cut.exists(_.suit == suits.head)) Map((cards + cut.get) -> (cards.size + 1))
      else Map(cards -> cards.size)
    } else {
      Map()
    }

  private def jack(cards: Set[Card], cut: Card): Map[Set[Card], Int] =
    cards.find(c => c.value == 11 && c.suit == cut.suit).map(c => Map(Set(c) -> 1)).getOrElse(Map())
}
