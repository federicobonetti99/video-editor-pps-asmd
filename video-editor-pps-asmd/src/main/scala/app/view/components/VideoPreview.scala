package app.view.components

import core.engine.EffectCalculator
import core.model.VideoEffect
import javafx.scene.effect.{ColorAdjust, SepiaTone}
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.layout.StackPane
import scalafx.scene.media.MediaView

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

case class PlaybackState(
                          mediaUrl: Option[String],
                          relativeTime: Double,
                          isPlaying: Boolean,
                          effect: VideoEffect = VideoEffect.None,
                          clipDuration: Double = 0.0
                        )

class VideoPreview(width: Double, height: Double) extends StackPane:

  alignment = Pos.Center
  prefWidth = width
  prefHeight = height
  minHeight = height
  maxHeight = height
  style = "-fx-background-color: black; -fx-border-color: #333333; -fx-border-width: 2px;"

  private val jfxMediaView = new javafx.scene.media.MediaView()
  private val mediaView = new MediaView(jfxMediaView):
    fitWidth = width
    fitHeight = height
    preserveRatio = true

  children = Seq(mediaView)

  private val activeJfxPlayer = new AtomicReference[Option[javafx.scene.media.MediaPlayer]](None)
  private val currentLoadedUrl = new AtomicReference[Option[String]](None)
  private val isSeekingOrLoading = new AtomicBoolean(false)

  def update(state: PlaybackState, onTimeUpdated: Double => Unit): Unit =
    Platform.runLater:
      state.mediaUrl match
        case Some(url) =>
          applyVisualEffects(state.effect, state.relativeTime, state.clipDuration)
          val targetDuration = javafx.util.Duration.seconds(state.relativeTime)

          if !currentLoadedUrl.get().contains(url) then
            isSeekingOrLoading.set(true)

            activeJfxPlayer.get().foreach: p =>
              p.setOnEndOfMedia(null)
              p.stop()
              p.dispose()
            activeJfxPlayer.set(None)
            jfxMediaView.setMediaPlayer(null)
            currentLoadedUrl.set(Some(url))

            try
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)

              jfxPlayer.setMute(true)
              jfxPlayer.setVolume(0.0)

              jfxMediaView.setMediaPlayer(jfxPlayer)
              activeJfxPlayer.set(Some(jfxPlayer))

              jfxPlayer.currentTimeProperty().addListener: (_, _, newTime) =>
                if !isSeekingOrLoading.get() && jfxPlayer.getStatus == javafx.scene.media.MediaPlayer.Status.PLAYING then
                  onTimeUpdated(newTime.toSeconds)

              jfxPlayer.setOnReady: () =>
                jfxPlayer.seek(targetDuration)
                Platform.runLater:
                  isSeekingOrLoading.set(false)
                  if state.isPlaying then jfxPlayer.play()
                  else jfxPlayer.pause()

              jfxPlayer.setOnEndOfMedia: () =>
                val duration = jfxMedia.getDuration
                if duration != null && !duration.isUnknown then
                  onTimeUpdated(duration.toSeconds)
            catch
              case _: Exception =>
                isSeekingOrLoading.set(false)

          else
            activeJfxPlayer.get().foreach: player =>
              if !isSeekingOrLoading.get() then
                val status = player.getStatus
                val currentSec = player.getCurrentTime.toSeconds
                val diff = Math.abs(currentSec - state.relativeTime)

                if state.isPlaying then
                  if status != javafx.scene.media.MediaPlayer.Status.PLAYING then
                    if diff > 0.1 then player.seek(targetDuration)
                    player.play()
                  else if diff > 0.4 then
                    player.seek(targetDuration)
                else
                  if status == javafx.scene.media.MediaPlayer.Status.PLAYING then
                    player.pause()
                  if diff > 0.03 then
                    player.seek(targetDuration)

        case None =>
          resetVisualEffects()
          isSeekingOrLoading.set(false)
          activeJfxPlayer.get().foreach: p =>
            p.stop()
            p.dispose()
          activeJfxPlayer.set(None)
          currentLoadedUrl.set(None)
          jfxMediaView.setMediaPlayer(null)

  private def applyVisualEffects(effect: VideoEffect, relativeTime: Double, duration: Double): Unit =
    val transform = EffectCalculator.computeTransform(effect, relativeTime, duration)
    jfxMediaView.setScaleX(transform.scale)
    jfxMediaView.setScaleY(transform.scale)
    jfxMediaView.setTranslateX(transform.translateX)
    jfxMediaView.setTranslateY(transform.translateY)
    jfxMediaView.setOpacity(transform.opacity)

    effect match
      case VideoEffect.Grayscale =>
        val ca = new ColorAdjust()
        ca.setSaturation(-1.0)
        jfxMediaView.setEffect(ca)

      case VideoEffect.Sepia =>
        val sepia = new SepiaTone()
        sepia.setLevel(0.8)
        jfxMediaView.setEffect(sepia)

      case VideoEffect.Brightness(level) =>
        val ca = new ColorAdjust()
        ca.setBrightness(level)
        jfxMediaView.setEffect(ca)

      case VideoEffect.Invert =>
        val ca = new ColorAdjust()
        ca.setHue(1.0)
        ca.setContrast(-1.0)
        jfxMediaView.setEffect(ca)

      case _ =>
        jfxMediaView.setEffect(null)

  private def resetVisualEffects(): Unit =
    jfxMediaView.setScaleX(1.0)
    jfxMediaView.setScaleY(1.0)
    jfxMediaView.setTranslateX(0.0)
    jfxMediaView.setTranslateY(0.0)
    jfxMediaView.setOpacity(1.0)
    jfxMediaView.setEffect(null)