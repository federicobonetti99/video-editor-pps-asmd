package app.utils

import scalafx.stage.FileChooser
import scalafx.stage.Window
import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer

object MediaImporter:

  def chooseVideoFile(parentWindow: Window): Option[(File, Double)] =
    val fileChooser = new FileChooser {
      title = "Seleziona un file Video"
      extensionFilters.add(new FileChooser.ExtensionFilter("Video Files", Seq("*.mp4", "*.mkv", "*.avi")))
    }

    Option(fileChooser.showOpenDialog(parentWindow)).map { file =>
      val duration = try {
        extractMp4Duration(file)
      } catch {
        case e: Exception =>
          println(s"⚠️ Impossibile analizzare i byte del file. Fallback a 10 min: ${e.getMessage}")
          600.0
      }
      (file, duration)
    }

  private def extractMp4Duration(file: File): Double =
    val raf = new RandomAccessFile(file, "r")
    try {
      val channel = raf.getChannel
      val size = channel.size()
      val buffer = ByteBuffer.allocate(64)
      var foundMvhd = false
      var position = 0L

      val target = Array[Byte](109, 118, 104, 100)
      val searchBuffer = ByteBuffer.allocate(1024 * 64)

      var bytesRead = channel.read(searchBuffer)
      var filePointer = 0L

      while (bytesRead > 0 && !foundMvhd && filePointer < size) {
        val bytes = searchBuffer.array()
        var i = 0
        while (i < bytesRead - 4 && !foundMvhd) {
          if (bytes(i) == target(0) && bytes(i+1) == target(1) && bytes(i+2) == target(2) && bytes(i+3) == target(3)) {
            foundMvhd = true
            position = filePointer + i
          }
          i += 1
        }
        if (!foundMvhd) {
          filePointer += bytesRead - 4
          channel.position(filePointer)
          searchBuffer.clear()
          bytesRead = channel.read(searchBuffer)
        }
      }

      if (foundMvhd) {
        channel.position(position + 4)
        val dataBuf = ByteBuffer.allocate(32)
        channel.read(dataBuf)
        dataBuf.flip()

        val version = dataBuf.get()
        dataBuf.get(); dataBuf.get(); dataBuf.get()

        if (version == 1) {
          dataBuf.getLong()
          dataBuf.getLong()
          val timescale = dataBuf.getInt() & 0xFFFFFFFFL
          val duration = dataBuf.getLong()
          duration.toDouble / timescale.toDouble
        } else {
          dataBuf.getInt()
          dataBuf.getInt()
          val timescale = dataBuf.getInt() & 0xFFFFFFFFL
          val duration = dataBuf.getInt() & 0xFFFFFFFFL
          duration.toDouble / timescale.toDouble
        }
      } else {
        throw new Exception("Atomo 'mvhd' non trovato nel file.")
      }
    } finally {
      raf.close()
    }