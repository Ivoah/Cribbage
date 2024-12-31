case class Hand(cards: Seq[Card]) {
  def score(cut: Card, crib: Boolean = false): Map[String, Int] = {
    Map(
      "fifteens" -> fifteens(cards :+ cut),
      "pairs" -> pairs(cards :+ cut),
      "runs" -> run(cards :+ cut),
      "flush" -> (if (crib) flush(cards :+ cut) else flush(cards, Some(cut))),
      "jack" -> jack(cards, cut)
    )
  }

  private def fifteens(cards: Seq[Card]): Int =
    (1 to cards.length).flatMap(n => cards.combinations(n)).count(_.map(_.scoreValue).sum == 15)*2

  private def pairs(cards: Seq[Card]): Int =
    cards.combinations(2).count{case Seq(c1, c2) => c1.value == c2.value}*2

  private def run(cards: Seq[Card]): Int = {
    val runs = (3 to cards.length).flatMap(n => cards.combinations(n).filter(_.map(_.value).sorted.sliding(n).exists(_.sliding(2).forall{case Seq(c1, c2) => c1 + 1 == c2})))
    runs.filterNot(s => runs.exists(l => s.length < l.length && s.forall(l.contains))).map(_.length).sum
  }

  private def flush(cards: Seq[Card], cut: Option[Card] = None): Int =
    if (cards.forall(_.suite == cards.head.suite)) cards.length + (if (cut.exists(_.suite == cards.head.suite)) 1 else 0) else 0

  private def jack(cards: Seq[Card], cut: Card): Int =
    if (cards.exists(c => c.value == 11 && c.suite == cut.suite)) 1 else 0
}

@main
def handTest(): Unit = {
  val hand = util.Random.shuffle(Seq(Card(1, Suite.Spades), Card(2, Suite.Hearts), Card(3, Suite.Diamonds), Card(3, Suite.Spades), Card(4, Suite.Spades)))
  println(hand)
  println(Hand(hand.take(4)).score(hand.last))
}
