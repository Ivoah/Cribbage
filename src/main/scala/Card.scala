import scala.language.implicitConversions

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
  val names = Seq(
    "Ace", "Two", "Three", "Four",
    "Five", "Six", "Seven", "Eight",
    "Nine", "Ten", "Jack", "Queen", "King"
  )

  def apply(value: String, suite: Suite.Value): Card = Card(names.indexOf(value) + 1, suite)
}
