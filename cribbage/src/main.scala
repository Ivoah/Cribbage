import scala.util.Try
import scala.util.Success
import scala.util.Failure

@main
def gui(): Unit = {
  CribbageGUI().open()
}

@main
def webhook(): Unit = {
  import net.ivoah.vial.*

  Server(
    Router {
      case ("POST", _, request) =>
        val payload = ujson.read(request.body)
        val rawHand = payload("payload")("message").str
        val phoneNumber = payload("payload")("phoneNumber").str

        val cards = Try(rawHand.split(" ").map(Card.fromShortName).toSeq)

        val score = cards match {
          case Success(cards) if cards.length == 5 =>
            val hand = Hand(cards.take(4).toSet)
            val cut = cards.last
            val scores = hand.score(cut)
            s"""${scores.flatMap {
              case (t, sets) => sets.map{case (s, v) => s"$t for $v: ${s.mkString(", ")}"}
            }.toSeq.mkString("\n")}
            |Total: ${scores.flatMap(_._2.map(_._2)).sum}""".stripMargin
          case Success(_) =>
            "Hand must be 4 cards followed by cut card"
          case Failure(_) =>
            "Could not parse cards"
        }

        requests.post(
          "http://192.168.1.160:8080/message",
          auth = requests.RequestAuth.Basic("sms", "hPoDs2nU"),
          data = upickle.default.stream(ujson.Obj("message" -> score, "phoneNumbers" -> ujson.Arr(phoneNumber)))
        )
        Response(score)
    },
    "127.0.0.1", 2000
  ).serve()
}

@main
def cmd(args: String*): Unit = {
  val cards = Try(args.mkString(" ").split(" ").map(Card.fromShortName).toSeq)

  cards match {
    case Success(cards) if cards.length == 5 =>
      val hand = Hand(cards.init.toSet)
      val cut = cards.last
      val scores = hand.score(cut)
      println(scores.flatMap {
        case (t, sets) => sets.map{case (s, v) => s"$t for $v: ${s.mkString(", ")}"}
      }.toSeq.mkString("\n"))
      println(s"Total: ${scores.flatMap(_._2.map(_._2)).sum}")
    case Success(_) =>
      println("Hand must be 4 cards followed by cut card")
    case Failure(_) =>
      println("Could not parse cards")
  }
}

@main
def random(): Unit = {
  val cards = util.Random.shuffle(Card.fullDeck).take(5)
  val hand = Hand(cards.init.toSet)
  val scores = hand.score(cards.last)
  println(cards.map(_.shortName).mkString(" "))
  println(scores.flatMap {
    case (t, sets) => sets.map{case (s, v) => s"$t for $v: ${s.mkString(", ")}"}
  }.toSeq.mkString("\n"))
  println(s"Total: ${scores.flatMap(_._2.map(_._2)).sum}")
}

@main
def handTest(): Unit = {
  val hands = Seq(
    Seq(Card(1, Suit.Spades), Card(2, Suit.Hearts), Card(3, Suit.Diamonds), Card(3, Suit.Spades), Card(4, Suit.Spades)), // 2 runs of 4
    Seq(Card(13, Suit.Hearts), Card(5, Suit.Hearts), Card(5, Suit.Clubs), Card(10, Suit.Clubs), Card(3, Suit.Diamonds)), // 4 fifteens
    Seq(Card(1, Suit.Hearts), Card(2, Suit.Hearts), Card(3, Suit.Hearts), Card(8, Suit.Hearts), Card(9, Suit.Clubs)), // Flush without cut
    Seq(Card(1, Suit.Hearts), Card(2, Suit.Hearts), Card(3, Suit.Hearts), Card(8, Suit.Hearts), Card(9, Suit.Hearts)), // Flush with cut
    Seq(Card(5, Suit.Clubs), Card(9, Suit.Diamonds), Card(11, Suit.Diamonds), Card(7, Suit.Hearts), Card(3, Suit.Diamonds)) // Right jack
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
