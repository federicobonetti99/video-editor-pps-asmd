package app.view.components

import app.ai.domain.SceneAsset
import scalafx.Includes.*
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, ProgressIndicator, ScrollPane, TextArea}
import scalafx.scene.layout.{HBox, Priority, VBox}

final class AIDirectorPanel(onGenerateStory: String => Unit) extends VBox:

  spacing = 8
  padding = Insets(10, 12, 10, 12)
  minWidth = 0
  prefHeight = 270
  maxHeight = 270

  private val titleLabel = new Label("Regista AI • Storyboard"):
    style = "-fx-text-fill: #f0f0f0; -fx-font-size: 13px; -fx-font-weight: bold;"

  private val hintLabel = new Label("Descrivi una storia o l'idea di un video: verrà divisa in scene e montata in sequenza."):
    wrapText = true
    style = "-fx-text-fill: #9a9a9a; -fx-font-size: 11px;"

  private val storyArea = new TextArea:
    promptText = "Es. Un piccolo gatto astronauta arancione atterra su un pianeta di formaggio, incontra un topo robot amichevole e costruiscono un razzo..."
    wrapText = true
    prefRowCount = 4
    vgrow = Priority.Always
    onKeyReleased = e => e.consume()
    style =
      """-fx-control-inner-background: #252526;
        |-fx-background-color: #3c3c3c;
        |-fx-text-fill: #e8e8e8;
        |-fx-prompt-text-fill: #777777;
        |-fx-border-color: #454545;
        |-fx-border-radius: 4px;
        |-fx-background-radius: 4px;
        |-fx-font-size: 11px;""".stripMargin

  private val generateButton = new Button("Genera Storyboard"):
    maxWidth = Double.MaxValue
    focusTraversable = false
    style =
      """-fx-background-color: #2563eb;
        |-fx-text-fill: white;
        |-fx-font-weight: bold;
        |-fx-padding: 6 12 6 12;
        |-fx-background-radius: 4px;
        |-fx-cursor: hand;""".stripMargin
    onAction = _ =>
      val story = storyArea.text.value.trim
      if story.nonEmpty then
        setLoading(true, "Regista Groq al lavoro...")
        onGenerateStory(story)

  private val spinner = new ProgressIndicator:
    prefWidth = 14
    prefHeight = 14
    visible = false
    managed = false

  private val statusLabel = new Label(""):
    wrapText = true
    style = "-fx-text-fill: #9cdcfe; -fx-font-size: 11px;"

  private val statusRow = new HBox(6, spinner, statusLabel):
    alignment = Pos.CenterLeft

  private val scenesBox = new VBox(4)

  private val scenesScroll = new ScrollPane:
    content = scenesBox
    fitToWidth = true
    prefHeight = 65
    maxHeight = 65
    visible = false
    managed = false
    style = "-fx-background: #1e1e1e; -fx-background-color: #1e1e1e; -fx-border-color: #333333;"

  children = Seq(titleLabel, hintLabel, storyArea, generateButton, statusRow, scenesScroll)

  def setLoading(loading: Boolean, message: String = ""): Unit =
    storyArea.disable = loading
    generateButton.disable = loading
    generateButton.text = if loading then "Creazione scene..." else "Genera Storyboard"
    spinner.visible = loading
    spinner.managed = loading
    statusLabel.style = "-fx-text-fill: #9cdcfe; -fx-font-size: 11px;"
    statusLabel.text = message

  def showError(message: String): Unit =
    setLoading(false)
    statusLabel.style = "-fx-text-fill: #f48771; -fx-font-size: 11px;"
    statusLabel.text = message

  def showGeneratedScenes(assets: List[SceneAsset]): Unit =
    storyArea.text = ""
    setLoading(false, s"Pronto: ${assets.size} scene aggiunte in timeline.")
    statusLabel.style = "-fx-text-fill: #89d185; -fx-font-size: 11px;"
    scenesBox.children.clear()
    assets.foreach { asset =>
      val d = asset.directive
      val item = new Label(s"${d.sceneNumber}. [${d.shotType}]${d.narration}"):
        wrapText = true
        style = "-fx-text-fill: #cccccc; -fx-font-size: 10px; -fx-background-color: #262626; -fx-padding: 4; -fx-background-radius: 3;"
      scenesBox.children.add(item)
    }
    val hasScenes = assets.nonEmpty
    scenesScroll.visible = hasScenes
    scenesScroll.managed = hasScenes