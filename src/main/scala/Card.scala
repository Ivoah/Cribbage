object Suit extends Enumeration {
  val Diamonds, Hearts, Spades, Clubs = Value
}

case class Card(value: Int, suit: Suit.Value) {
  private val name = Card.names(value - 1)
  override def toString: String = s"$name of $suit"

  val scoreValue: Int = if (value >= 10) 10 else value

  def +(other: Card): Int = scoreValue + other.scoreValue
}

object Card {
  private val shortRe = raw"(?i)(\d{1,2}|[AJQK])([HCSD])".r

  extension (seq: Seq[Card]) {
    def isRun(): Boolean = seq.map(_.value).sliding(2).forall{case Seq(c1, c2) => c1 + 1 == c2}
  }

  val names = Seq(
    "Ace", "Two", "Three", "Four",
    "Five", "Six", "Seven", "Eight",
    "Nine", "Ten", "Jack", "Queen", "King"
  )

  val fullDeck = for (s <- Suit.values.toSeq.sorted; v <- names) yield Card(v, s)

  def apply(value: String, suit: Suit.Value): Card = Card(names.indexOf(value) + 1, suit)

  def fromShortName(name: String): Card = {
    name match {
      case shortRe(value, suit) => Card(
        value.toIntOption.getOrElse(value.toUpperCase() match {
          case "A" => 1
          case "J" => 11
          case "Q" => 12
          case "K" => 13
        }),
        suit.toUpperCase() match {
          case "D" => Suit.Diamonds
          case "H" => Suit.Hearts
          case "S" => Suit.Spades
          case "C" => Suit.Clubs
        }
      )
    }
  }
}

implicit object CardOrdering extends Ordering[Card] {
 def compare(c1: Card, c2: Card) = c1.value.compare(c2.value)
}
