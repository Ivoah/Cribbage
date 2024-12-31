object Suite extends Enumeration {
  val Diamonds, Hearts, Spades, Clubs = Value
}

case class Card(value: Int, suite: Suite.Value) {
  private val name = Card.names(value - 1)
  override def toString: String = s"$name of $suite"

  val scoreValue: Int = if (value >= 10) 10 else value

  def +(other: Card): Int = scoreValue + other.scoreValue
}

object Card {
  extension (seq: Seq[Card]) {
    def isRun(): Boolean = seq.map(_.value).sliding(2).forall{case Seq(c1, c2) => c1 + 1 == c2}
  }

  val names = Seq(
    "Ace", "Two", "Three", "Four",
    "Five", "Six", "Seven", "Eight",
    "Nine", "Ten", "Jack", "Queen", "King"
  )

  val fullDeck = for (s <- Suite.values.toSeq.sorted; v <- names) yield Card(v, s)

  def apply(value: String, suite: Suite.Value): Card = Card(names.indexOf(value) + 1, suite)
}

implicit object CardOrdering extends Ordering[Card] {
 def compare(c1: Card, c2: Card) = c1.value.compare(c2.value)
}
