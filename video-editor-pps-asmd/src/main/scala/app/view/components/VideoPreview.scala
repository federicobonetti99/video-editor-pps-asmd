package app.view.components

import core.engine.EffectCalculator
import core.model.VideoEffect
import javafx.scene.effect.{Blend, BlendMode, ColorAdjust, ColorInput, SepiaTone}
import javafx.scene.paint.Color
import org.bytedeco.javacv.{FFmpegFrameGrabber, JavaFXFrameConverter}
import scalafx.Includes.*
import scalafx.animation.AnimationTimer
import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.StackPane

import java.net.URI
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

  private val imageView = new ImageView():
    fitWidth = width
    fitHeight = height
    preserveRatio = true
    visible = true

  children = Seq(imageView)

  private val cachedInvertEffect: Blend =
    val whiteInput = new ColorInput(0, 0, width, height, Color.WHITE)
    val blend = new Blend()
    blend.setMode(BlendMode.DIFFERENCE)
    blend.setTopInput(whiteInput)
    blend

  private val currentGrabber = new AtomicReference[Option[FFmpegFrameGrabber]](None)
  private val currentLoadedUrl = new AtomicReference[Option[String]](None)
  private val converter = new JavaFXFrameConverter()

  private val lastRenderedSecond = new AtomicReference[Double](-1.0)
  private val lastFrameNanos = new AtomicReference[Long](0L)
  private val playbackLoop = new AtomicReference[Option[AnimationTimer]](None)
  private val wasPlaying = new AtomicBoolean(false)
  private val currentPlaybackTime = new AtomicReference[Double](0.0)

  private val imageExtensions = Set("png", "jpg", "jpeg", "bmp", "gif", "webp")

  private def isImageUrl(url: String): Boolean =
    val cleanUrl = url.toLowerCase.takeWhile(_ != '?')
    imageExtensions.exists(ext => cleanUrl.endsWith(s".$ext"))

  private def extractFilePath(url: String): String =
    try
      val uri = new URI(url)
      if uri.getScheme == "file" then
        val path = uri.getPath
        if path.startsWith("/") && path.length > 2 && path.charAt(2) == ':' then
          path.substring(1)
        else
          path
      else
        url
    catch
      case _: Exception => url

  def update(state: PlaybackState, onTimeUpdated: Double => Unit): Unit =
    Platform.runLater:
      state.mediaUrl match
        case Some(url) =>
          val isImage = isImageUrl(url)
          applyVisualEffects(imageView, state.effect, state.relativeTime, state.clipDuration)

          if isImage then
            renderImage(url)
          else
            renderVideo(url, state, onTimeUpdated)

        case None =>
          clearPreview()

  private def renderImage(url: String): Unit =
    stopPlaybackLoop()
    disposeGrabber()

    if !currentLoadedUrl.get().contains(url) then
      currentLoadedUrl.set(Some(url))
      try
        imageView.image = new Image(url)
      catch
        case _: Exception =>
          imageView.image = null

  private def renderVideo(url: String, state: PlaybackState, onTimeUpdated: Double => Unit): Unit =
    if !currentLoadedUrl.get().contains(url) then
      disposeGrabber()
      currentLoadedUrl.set(Some(url))
      lastRenderedSecond.set(-1.0)
      wasPlaying.set(false)

      try
        val localPath = extractFilePath(url)
        val grabber = new FFmpegFrameGrabber(localPath)
        grabber.start()
        currentGrabber.set(Some(grabber))
      catch
        case _: Exception =>
          currentGrabber.set(None)

    currentGrabber.get().foreach: grabber =>
      if state.isPlaying then
        if !wasPlaying.get() || Math.abs(lastRenderedSecond.get() - state.relativeTime) > 0.25 then
          seekFrame(grabber, state.relativeTime)

        wasPlaying.set(true)

        if playbackLoop.get().isEmpty then
          currentPlaybackTime.set(state.relativeTime)
          lastFrameNanos.set(System.nanoTime())

          val loop = AnimationTimer { now =>
            val previousNanos = lastFrameNanos.getAndSet(now)
            val delta = (now - previousNanos) / 1e9
            val newTime = currentPlaybackTime.updateAndGet(t => t + delta)

            if state.clipDuration > 0 && newTime >= state.clipDuration then
              currentPlaybackTime.set(state.clipDuration)
              onTimeUpdated(state.clipDuration)
              stopPlaybackLoop()
            else
              onTimeUpdated(newTime)
              val targetMicros = (newTime * 1000000.0).toLong
              if grabber.getTimestamp < targetMicros then
                val frame = grabber.grabImage()
                if frame != null then
                  val fxImg = converter.convert(frame)
                  if fxImg != null then
                    imageView.image = fxImg
                    lastRenderedSecond.set(newTime)
          }
          playbackLoop.set(Some(loop))
          loop.start()
      else
        wasPlaying.set(false)
        stopPlaybackLoop()
        if Math.abs(lastRenderedSecond.get() - state.relativeTime) > 0.03 then
          seekFrame(grabber, state.relativeTime)

  private def seekFrame(grabber: FFmpegFrameGrabber, seconds: Double): Unit =
    try
      val targetMicros = (Math.max(0.0, seconds) * 1000000.0).toLong
      grabber.setTimestamp(targetMicros)
      val frame = grabber.grabImage()
      if frame != null then
        val fxImage = converter.convert(frame)
        if fxImage != null then
          imageView.image = fxImage
          lastRenderedSecond.set(seconds)
    catch
      case _: Exception => ()

  private def stopPlaybackLoop(): Unit =
    playbackLoop.getAndSet(None).foreach(_.stop())

  private def disposeGrabber(): Unit =
    currentGrabber.get().foreach: g =>
      try
        g.stop()
        g.release()
      catch
        case _: Exception => ()
    currentGrabber.set(None)
    currentLoadedUrl.set(None)

  private def clearPreview(): Unit =
    stopPlaybackLoop()
    disposeGrabber()
    resetVisualEffects(imageView)
    imageView.image = null
    wasPlaying.set(false)

  private def applyVisualEffects(node: scalafx.scene.Node, effect: VideoEffect, relativeTime: Double, duration: Double): Unit =
    val transform = EffectCalculator.computeTransform(effect, relativeTime, duration)
    node.scaleX = transform.scale
    node.scaleY = transform.scale
    node.translateX = transform.translateX
    node.translateY = transform.translateY
    node.opacity = transform.opacity

    effect match
      case VideoEffect.Grayscale =>
        val ca = new ColorAdjust()
        ca.setSaturation(-1.0)
        node.effect = ca

      case VideoEffect.Sepia =>
        val sepia = new SepiaTone()
        sepia.setLevel(0.8)
        node.effect = sepia

      case VideoEffect.Brightness(level) =>
        val ca = new ColorAdjust()
        ca.setBrightness(level)
        node.effect = ca

      case VideoEffect.Invert =>
        node.effect = cachedInvertEffect

      case _ =>
        node.effect = null

  private def resetVisualEffects(node: scalafx.scene.Node): Unit =
    node.scaleX = 1.0
    node.scaleY = 1.0
    node.translateX = 0.0
    node.translateY = 0.0
    node.opacity = 1.0
    node.effect = null