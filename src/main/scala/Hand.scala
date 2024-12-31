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
    val suites = cards.map(_.suite)
    if (suites.size == 1) {
      if (cut.exists(_.suite == suites.head)) Map((cards + cut.get) -> (cards.size + 1))
      else Map(cards -> cards.size)
    } else {
      Map()
    }

  private def jack(cards: Set[Card], cut: Card): Map[Set[Card], Int] =
    cards.find(c => c.value == 11 && c.suite == cut.suite).map(c => Map(Set(c) -> 1)).getOrElse(Map())
}

@main
def handTest(): Unit = {
  val hands = Seq(
    Seq(Card(1, Suite.Spades), Card(2, Suite.Hearts), Card(3, Suite.Diamonds), Card(3, Suite.Spades), Card(4, Suite.Spades)), // 2 runs of 4
    Seq(Card(13, Suite.Hearts), Card(5, Suite.Hearts), Card(5, Suite.Clubs), Card(10, Suite.Clubs), Card(3, Suite.Diamonds)), // 4 fifteens
    Seq(Card(1, Suite.Hearts), Card(2, Suite.Hearts), Card(3, Suite.Hearts), Card(8, Suite.Hearts), Card(9, Suite.Clubs)), // Flush without cut
    Seq(Card(1, Suite.Hearts), Card(2, Suite.Hearts), Card(3, Suite.Hearts), Card(8, Suite.Hearts), Card(9, Suite.Hearts)), // Flush with cut
    Seq(Card(5, Suite.Clubs), Card(9, Suite.Diamonds), Card(11, Suite.Diamonds), Card(7, Suite.Hearts), Card(3, Suite.Diamonds)) // Right jack
  )
  for (hand <- hands) {
    println(s"Hand: [${hand.mkString(", ")}]")
    val scores = Hand(hand.take(4).toSet).score(hand.last)
    for ((t, set) <- scores) {
      println(s"$t: ${set.map(cards => s"[${cards._1.mkString(", ")}]").mkString(", ")}")
    }
    println("---")
  }
}
