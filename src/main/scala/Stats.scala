object Stats extends App {
  val hand = util.Random.shuffle(Card.fullDeck).take(5)
  println(hand)
}
