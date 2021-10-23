import scala.swing._

import com.github.weisj.darklaf.LafManager

object CribbageGUI extends MainFrame with App {
  LafManager.installTheme(LafManager.getPreferredThemeStyle)
  LafManager.enabledPreferenceChangeReporting(true)
  LafManager.addThemePreferenceChangeListener(e => LafManager.installTheme(e.getPreferredThemeStyle))

  private val starter = (new ComboBox(Card.names), new ComboBox(Suite.values.toSeq))
  private val cards = (1 to 4).map { _ =>
    (new ComboBox(Card.names), new ComboBox(Suite.values.toSeq))
  }

  contents = new BorderPanel() {
    layout(new BoxPanel(Orientation.Vertical) {
      contents ++= cards.map { boxes =>
        new BoxPanel(Orientation.Horizontal) {
          contents ++= Seq(boxes._1, new Label(" of "), boxes._2)
        }
      }
    }) = BorderPanel.Position.Center
    layout(new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new Separator(),
        new BoxPanel(Orientation.Horizontal) {
          val action: Action = Action("Score") {
            val hand = Hand(cards.map(c => Card(c._1.selection.item, c._2.selection.item)))
            val score = hand.score(Card(starter._1.selection.item, starter._2.selection.item))
            action.title = s"Score: $score"
            pack()
          }
          contents ++= Seq(
            starter._1, new Label(" of "), starter._2,
            new Button(action)
          )
        }
      )
    }) = BorderPanel.Position.South
  }

  title = "Cribbage Scorer"
  resizable = false
  centerOnScreen()
  open()
}
