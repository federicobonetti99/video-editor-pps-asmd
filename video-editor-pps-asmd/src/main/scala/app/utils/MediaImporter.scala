package app.utils

import scalafx.stage.FileChooser
import scalafx.stage.Window
import java.io.File
import scala.util.Try
import scala.sys.process.*

enum ImportedMedia:
  case Video(file: File, duration: Double)
  case Audio(file: File, duration: Double)

object MediaImporter:

  private val videoExts = Set("mp4", "mkv", "avi", "mov")
  private val audioExts = Set("mp3", "wav", "aac", "m4a", "ogg")

  private def toFilterPatterns(exts: Set[String]): Seq[String] =
    exts.toSeq.flatMap(ext => Seq(s"*.$ext", s"*.${ext.toUpperCase}"))

  def chooseVideoFile(parentWindow: Window): Option[(File, Double)] =
    val fileChooser = new FileChooser:
      title = "Seleziona un file Video"
      extensionFilters.add(new FileChooser.ExtensionFilter("Video Files", toFilterPatterns(videoExts)))

    Option(fileChooser.showOpenDialog(parentWindow)).map(file => (file, getDuration(file)))

  def chooseAudioFile(parentWindow: Window): Option[(File, Double)] =
    val fileChooser = new FileChooser:
      title = "Seleziona un file Audio"
      extensionFilters.add(new FileChooser.ExtensionFilter("Audio Files", toFilterPatterns(audioExts)))

    Option(fileChooser.showOpenDialog(parentWindow)).map(file => (file, getDuration(file)))

  def chooseGenericMedia(parentWindow: Window): Option[ImportedMedia] =
    val fileChooser = new FileChooser:
      title = "Seleziona file Multimediale"
      extensionFilters.addAll(
        new FileChooser.ExtensionFilter("Media Files", toFilterPatterns(videoExts ++ audioExts)),
        new FileChooser.ExtensionFilter("Video Files", toFilterPatterns(videoExts)),
        new FileChooser.ExtensionFilter("Audio Files", toFilterPatterns(audioExts))
      )

    Option(fileChooser.showOpenDialog(parentWindow)).flatMap { file =>
      val ext = file.getName.split('.').lastOption.map(_.toLowerCase).getOrElse("")
      val duration = getDuration(file)

      if videoExts.contains(ext) then Some(ImportedMedia.Video(file, duration))
      else if audioExts.contains(ext) then Some(ImportedMedia.Audio(file, duration))
      else None
    }

  private def getDuration(file: File): Double =
    extractDurationWithFFprobe(file).getOrElse(600.0)

  private def extractDurationWithFFprobe(file: File): Option[Double] =
    Try {
      val cmd = Seq(
        "ffprobe", "-v", "error",
        "-show_entries", "format=duration",
        "-of", "default=noprint_wrappers=1:nokey=1",
        file.getAbsolutePath
      )
      cmd.!!.trim.toDouble
    }.toOption