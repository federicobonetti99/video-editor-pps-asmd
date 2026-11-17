package app.ai.services

import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import core.model.*
import core.engine.TimelineEngine
import core.model.{ImageClip, Timeline, VideoEffect}

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
        val effect = parseVideoEffect(effectName)
        val targetTrackOpt = timeline.videoTracks.find(_.id == trackId).orElse(timeline.videoTracks.headOption)
        targetTrackOpt match
          case Some(track) if track.clips.nonEmpty =>
            val idxAtCursor = track.clips.indexWhere(_.containsTime(timeline.currentTime))
            val targetIdx = if idxAtCursor != -1 then idxAtCursor else track.clips.length - 1
            AIActionOutcome.UpdatedTimeline(
              TimelineEngine.applyEffectToVideoClip(timeline, track.id, targetIdx, effect)
            )
          case _ =>
            break(AIGenerationError("Nessuna clip video presente su cui applicare l'effetto."))

      case UserIntent.RemoveSilences =>
        AIActionOutcome.UpdatedTimeline(TimelineEngine.snapAllTracks(timeline))

      case UserIntent.Unknown(raw) =>
        break(AIGenerationError(s"Comando non riconosciuto: $raw"))

  private def parseVideoEffect(name: String): VideoEffect =
    name.trim.toLowerCase match
      case "zoomin" | "zoom"      => VideoEffect.ZoomIn(1.25)
      case "shake" | "vibrazione" => VideoEffect.Shake(5.0, 10.0)
      case "fadein" | "dissolvenza" => VideoEffect.FadeIn(1.0)
      case _                      => VideoEffect.None