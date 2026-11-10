package app.view.components

import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Label, ProgressBar}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.layout.VBox
import scalafx.stage.{FileChooser, Modality, Stage, Window}
import java.io.File

class ExportProgressDialog(ownerWindow: Window):

  private val progressLabel = new Label("Preparazione esportazione..."):
    style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-text-fill: #ffffff;"

  private val progressBar = new ProgressBar:
    progress = 0.0
    prefWidth = 320
    prefHeight = 22

  private val dialogStage = new Stage:
    title = "Esportazione Video"
    initModality(Modality.ApplicationModal)
    if ownerWindow != null then initOwner(ownerWindow)
    resizable = false
    scene = new Scene(360, 120):
      root = new VBox(15, progressLabel, progressBar):
        padding = Insets(20)
        alignment = Pos.Center
        style = "-fx-background-color: #2b2b2b;"

  def show(): Unit = dialogStage.show()

  def close(): Unit = Platform.runLater(dialogStage.close())

  def updateProgress(fraction: Double): Unit =
    Platform.runLater:
      progressBar.progress = fraction
      val percent = Math.min(100, (fraction * 100).toInt)
      progressLabel.text = s"Esportazione in corso: $percent%"

object ExportDialogUtils:

  def showSaveFileDialog(ownerWindow: Window): Option[File] =
    val fileChooser = new FileChooser:
      title = "Export Timeline Video"
      extensionFilters.add(new FileChooser.ExtensionFilter("MP4 Video (*.mp4)", "*.mp4"))
    Option(fileChooser.showSaveDialog(ownerWindow))

  def showSuccessAlert(file: File): Unit =
    Platform.runLater:
      val alert = new Alert(AlertType.Information):
        title = "Export Completed"
        headerText = "Video Esportato con Successo"
        contentText = s"Salvato in: ${file.getAbsolutePath}"
      alert.showAndWait()

  def showErrorAlert(errorMessage: String): Unit =
    Platform.runLater:
      val alert = new Alert(AlertType.Error):
        title = "Export Error"
        headerText = "Errore durante l'esportazione FFmpeg"
        contentText = errorMessage
      alert.showAndWait()