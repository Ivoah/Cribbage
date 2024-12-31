import scala.swing._
import com.github.weisj.darklaf.LafManager

import javax.swing.border.EmptyBorder

object CribbageGUI extends MainFrame with App {
  LafManager.installTheme(LafManager.getPreferredThemeStyle)
  LafManager.enabledPreferenceChangeReporting(true)
  LafManager.addThemePreferenceChangeListener(e => LafManager.installTheme(e.getPreferredThemeStyle))

  private val starter = (new ComboBox(Card.names), new ComboBox(Suite.values.toSeq))
  private val cards = (1 to 4).map { _ =>
    (new ComboBox(Card.names), new ComboBox(Suite.values.toSeq))
  }

  private val scoresList = new ListView(List[(String, Int)]())

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
            val hand = Hand(cards.map(c => Card(c._1.selection.item, c._2.selection.item)))
            val scores = hand.score(Card(starter._1.selection.item, starter._2.selection.item))
            scoresList.listData = scores.toSeq
            action.title = s"Score: ${scores.values.sum}"
          }
          contents ++= Seq(
            new Label("Starter: "), starter._1, new Label(" of "), starter._2,
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
  open()
  print(size)
}
