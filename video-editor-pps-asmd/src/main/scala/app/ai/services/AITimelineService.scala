package app.ai.services

import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import core.engine.TimelineEngine
import core.model.*

import java.nio.file.Paths
import scala.util.boundary.{Label, break}

enum AIActionOutcome:
  case UpdatedTimeline(timeline: Timeline)
  case TriggerUndo

object AITimelineService:

  def processAndApply(
                       command: String,
                       timeline: Timeline,
                       trackId: Int = 1,
                       defaultDuration: Double = 5.0
                     )(using
                       parser: IntentParserCapability,
                       generator: ImageGeneratorCapability
                     )(using Label[AIGenerationError], Label[AIParseError]): AIActionOutcome =
    val intent = resolveIntentFastOrLLM(command)
    executeIntent(intent, timeline, trackId, defaultDuration)

  private def resolveIntentFastOrLLM(command: String)(using
                                                      parser: IntentParserCapability
  )(using Label[AIParseError]): UserIntent =
    command.trim.toLowerCase match
      case "undo" | "annulla" | "indietro" =>
        UserIntent.Undo
      case "cut" | "taglia" | "dividi" =>
        UserIntent.CutClip(-1.0)
      case "snap" | "compatta" | "togli silenzi" | "rimuovi silenzi" =>
        UserIntent.RemoveSilences
      case "muta" | "mute" | "muta audio" =>
        UserIntent.AdjustAudioVolume(AdjustDirection.Set, Some(0.0))
      case "alza volume" | "aumenta volume" =>
        UserIntent.AdjustAudioVolume(AdjustDirection.Increase, None)
      case "abbassa volume" | "riduci volume" =>
        UserIntent.AdjustAudioVolume(AdjustDirection.Decrease, None)
      case "aggiungi traccia video" | "nuova traccia video" =>
        UserIntent.AddTrack(TrackType.Video)
      case "aggiungi traccia audio" | "nuova traccia audio" =>
        UserIntent.AddTrack(TrackType.Audio)
      case _ =>
        parser.parse(command)

  private def executeIntent(
                             intent: UserIntent,
                             timeline: Timeline,
                             trackId: Int,
                             defaultDuration: Double
                           )(using
                             generator: ImageGeneratorCapability
                           )(using Label[AIGenerationError]): AIActionOutcome =
    intent match
      case UserIntent.Undo =>
        AIActionOutcome.TriggerUndo

      case UserIntent.GenerateImage(prompt) =>
        val generatedImage = generator.generateImage(prompt)
        val uriUrl =
          if generatedImage.path.startsWith("file:") then generatedImage.path
          else Paths.get(generatedImage.path).toAbsolutePath.toUri.toString
        val clip = ImageClip.create(
          sourceUrl = uriUrl,
          startTime = timeline.currentTime,
          duration = defaultDuration
        )
        AIActionOutcome.UpdatedTimeline(TimelineEngine.addVideoClip(timeline, trackId, clip))

      case UserIntent.CutClip(rawTimestamp) =>
        val cutTime = if rawTimestamp >= 0.0 then rawTimestamp else timeline.currentTime
        val cutVideoTimeline = timeline.videoTracks.foldLeft(timeline) { (acc, track) =>
          val idx = track.clips.indexWhere(_.containsTime(cutTime))
          if idx != -1 then
            TimelineEngine.cutVideoClip(acc, track.id, idx, cutTime - track.clips(idx).startTime)
          else acc
        }
        val cutFullTimeline = timeline.audioTracks.foldLeft(cutVideoTimeline) { (acc, track) =>
          val idx = track.clips.indexWhere(_.containsTime(cutTime))
          if idx != -1 then
            TimelineEngine.cutAudioClip(acc, track.id, idx, cutTime - track.clips(idx).startTime)
          else acc
        }
        AIActionOutcome.UpdatedTimeline(cutFullTimeline)

      case UserIntent.ApplyEffect(effectName) =>
        val effect = parseDefaultVideoEffect(effectName)
        val (targetTrackId, targetIdx, _) = findTargetVideoClip(timeline, trackId)
        AIActionOutcome.UpdatedTimeline(
          TimelineEngine.applyEffectToVideoClip(timeline, targetTrackId, targetIdx, effect)
        )

      case UserIntent.AdjustEffect(direction, parameter, amountOpt) =>
        val (targetTrackId, targetIdx, clip) = findTargetVideoClip(timeline, trackId)
        val updatedEffect = mutateExistingEffect(clip.effect, direction, parameter, amountOpt)
        AIActionOutcome.UpdatedTimeline(
          TimelineEngine.applyEffectToVideoClip(timeline, targetTrackId, targetIdx, updatedEffect)
        )

      case UserIntent.AdjustAudioVolume(direction, valueOpt) =>
        val targetTrackOpt = timeline.audioTracks.find(_.clips.nonEmpty)
        targetTrackOpt match
          case Some(track) =>
            val idxAtCursor = track.clips.indexWhere(_.containsTime(timeline.currentTime))
            val targetIdx = if idxAtCursor != -1 then idxAtCursor else track.clips.length - 1
            val currentVol = track.clips(targetIdx).volume
            val newVol = direction match
              case AdjustDirection.Set      => Math.max(0.0, valueOpt.getOrElse(1.0))
              case AdjustDirection.Increase => Math.min(3.0, currentVol + valueOpt.getOrElse(0.25))
              case AdjustDirection.Decrease => Math.max(0.0, currentVol - valueOpt.getOrElse(0.25))
            AIActionOutcome.UpdatedTimeline(
              TimelineEngine.applyVolumeToAudioClip(timeline, track.id, targetIdx, newVol)
            )
          case None =>
            break(AIGenerationError("Nessuna clip audio presente su cui modificare il volume."))

      case UserIntent.AddTrack(trackType) =>
        val updated = trackType match
          case TrackType.Video => TimelineEngine.addVideoTrack(timeline)
          case TrackType.Audio => TimelineEngine.addAudioTrack(timeline)
        AIActionOutcome.UpdatedTimeline(updated)

      case UserIntent.RemoveSilences =>
        AIActionOutcome.UpdatedTimeline(TimelineEngine.snapAllTracks(timeline))

      case UserIntent.Unknown(raw) =>
        break(AIGenerationError(s"Comando non riconosciuto: $raw"))

  private def findTargetVideoClip(timeline: Timeline, preferredTrackId: Int)(using Label[AIGenerationError]): (Int, Int, VisualClip) =
    val targetTrackOpt = timeline.videoTracks.find(_.id == preferredTrackId).orElse(timeline.videoTracks.headOption)
    targetTrackOpt match
      case Some(track) if track.clips.nonEmpty =>
        val idxAtCursor = track.clips.indexWhere(_.containsTime(timeline.currentTime))
        val targetIdx = if idxAtCursor != -1 then idxAtCursor else track.clips.length - 1
        (track.id, targetIdx, track.clips(targetIdx))
      case _ =>
        break(AIGenerationError("Nessuna clip video presente su cui applicare o modificare l'effetto."))

  private def parseDefaultVideoEffect(name: String): VideoEffect =
    name.trim.toLowerCase match
      case "zoomin" | "zoom"                             => VideoEffect.ZoomIn()
      case "shake" | "vibrazione" | "scuotimento"        => VideoEffect.Shake()
      case "fadein" | "dissolvenza"                      => VideoEffect.FadeIn()
      case "brightness" | "luminosita" | "luminosità"    => VideoEffect.Brightness()
      case "grayscale" | "biancoenero" | "bianco e nero" => VideoEffect.Grayscale
      case "sepia" | "seppia"                            => VideoEffect.Sepia
      case "invert" | "negativo"                         => VideoEffect.Invert
      case _                                             => VideoEffect.None

  private def mutateExistingEffect(
                                    current: VideoEffect,
                                    direction: AdjustDirection,
                                    parameter: Option[String],
                                    amountOpt: Option[Double]
                                  )(using Label[AIGenerationError]): VideoEffect =
    def step(base: Double, delta: Double, minVal: Double, maxVal: Double): Double =
      val raw = direction match
        case AdjustDirection.Set      => amountOpt.getOrElse(base)
        case AdjustDirection.Increase => base + valueOrDefault(amountOpt, delta)
        case AdjustDirection.Decrease => base - valueOrDefault(amountOpt, delta)
      Math.max(minVal, Math.min(maxVal, raw))

    def valueOrDefault(opt: Option[Double], defaultVal: Double): Double =
      opt.filter(_ > 0.0).getOrElse(defaultVal)

    current match
      case VideoEffect.ZoomIn(scale) =>
        VideoEffect.ZoomIn(targetScale = step(scale, 0.2, 1.05, 4.0))

      case VideoEffect.Shake(intensity, freq) =>
        parameter match
          case Some("frequency") =>
            VideoEffect.Shake(intensity = intensity, freq = step(freq, 4.0, 1.0, 60.0))
          case _ =>
            VideoEffect.Shake(intensity = step(intensity, 5.0, 1.0, 80.0), freq = freq)

      case VideoEffect.FadeIn(fadeDur) =>
        VideoEffect.FadeIn(fadeDuration = step(fadeDur, 0.5, 0.1, 10.0))

      case VideoEffect.Brightness(level) =>
        VideoEffect.Brightness(level = step(level, 0.15, -1.0, 1.0))

      case VideoEffect.None =>
        break(AIGenerationError("Nessun effetto attivo sulla clip da modificare. Applica prima un effetto (es. 'metti effetto shake')."))

      case other =>
        break(AIGenerationError(s"L'effetto $other non ha parametri regolabili."))