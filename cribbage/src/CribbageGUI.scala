import scala.swing.*
import com.github.weisj.darklaf.LafManager

import javax.swing.border.EmptyBorder

class CribbageGUI extends MainFrame {
  LafManager.install()
  LafManager.enabledPreferenceChangeReporting(true)
  LafManager.addThemePreferenceChangeListener(e => LafManager.installTheme(e.getPreferredThemeStyle))

  private val cut = (new ComboBox(Card.names), new ComboBox(Suit.values.toSeq))
  private val cards = (1 to 4).map { _ =>
    (new ComboBox(Card.names), new ComboBox(Suit.values.toSeq))
  }

  private val scoresList = new ListView(List[String]())

  contents = new BorderPanel() {
    layout(new BoxPanel(Orientation.Vertical) {
      contents ++= cards.map { boxes =>
        new BoxPanel(Orientation.Horizontal) {
          contents ++= Seq(boxes._1, new Label(" of "), boxes._2)
        }
      }
    }) = BorderPanel.Position.West
    layout(new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new Separator(),
        new BoxPanel(Orientation.Horizontal) {
          val action: Action = Action("Score") {
            val hand = Hand(cards.map(c => Card(c._1.selection.item, c._2.selection.item)).toSet)
            val scores = hand.score(Card(cut._1.selection.item, cut._2.selection.item))
            scoresList.listData = scores.flatMap {
              case (t, sets) => sets.map{case (s, v) => s"$t for $v: ${s.mkString(", ")}"}
            }.toSeq
            action.title = s"Score: ${scores.flatMap(_._2.map(_._2)).sum}"
          }
          contents ++= Seq(
            new Label("Cut: "), cut._1, new Label(" of "), cut._2,
            new Button(action)
          )
        }
      )
    }) = BorderPanel.Position.South
    layout(new ScrollPane(scoresList) {
      border = new EmptyBorder(0, 0, 0, 0)
    }) = BorderPanel.Position.Center
  }

  title = "Cribbage Scorer"
//  resizable = false
  minimumSize = size
  centerOnScreen()
}
