enum Suit(val unicodeBase: Int) {
  case Diamonds extends Suit(0x1F0A0)
  case Hearts extends Suit(0x1F0B0)
  case Spades extends Suit(0x1F0C0)
  case Clubs extends Suit(0x1F0D0)
}

case class Card(value: Int, suit: Suit) {
  private val name = Card.names(value - 1)
  override def toString: String = s"$name of $suit"
  def toUnicode: String = Character.toChars(suit.unicodeBase + value).mkString

  def shortName: String = {
    if (value > 1 && value < 10) s"${value.toString.head}${suit.toString.head}"
    else s"${name.head}${suit.toString.head}"
  }

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

  val fullDeck = for (s <- Suit.values; v <- names) yield Card(v, s)

  def apply(value: String, suit: Suit): Card = Card(names.indexOf(value) + 1, suit)

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
