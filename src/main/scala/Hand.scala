case class Hand(cards: Seq[Card]) {
  def score(starter: Card, crib: Boolean = false): Int = {
    fifteens(cards :+ starter) +
    pairs(cards :+ starter) +
    run(cards :+ starter) +
    (if (crib) flush(cards :+ starter) else flush(cards, Some(starter))) +
    jack(cards, starter)
  }

  private def fifteens(cards: Seq[Card]): Int =
    (1 to cards.length).flatMap(n => cards.combinations(n)).count(_.map(_.scoreValue).sum == 15)*2
  private def pairs(cards: Seq[Card]): Int =
    cards.combinations(2).count{case Seq(c1, c2) => c1.value == c2.value}*2
  private def run(cards: Seq[Card]): Int =
    (3 to cards.length).filter(n => cards.map(_.value).sorted.sliding(n).exists(_.sliding(2).forall{case Seq(c1, c2) => c1 + 1 == c2})).maxOption.getOrElse(0)
  private def flush(cards: Seq[Card], starter: Option[Card] = None): Int =
    (if (cards.forall(_.suite == cards.head.suite)) cards.length + (if (starter.exists(_.suite == cards.head.suite)) 1 else 0) else 0)
  private def jack(cards: Seq[Card], starter: Card): Int =
    if (cards.exists(c => c.value == 11 && c.suite == starter.suite)) 1 else 0
}
