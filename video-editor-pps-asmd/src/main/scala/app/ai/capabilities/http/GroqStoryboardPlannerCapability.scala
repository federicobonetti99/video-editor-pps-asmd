package app.ai.capabilities.http

import app.ai.capabilities.StoryboardPlannerCapability
import app.ai.domain.{AIDirectorError, SceneDirective, StoryboardPlan}

import java.net.URI
import java.net.http.{HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.util.Try
import scala.util.boundary.{Label, break}

final class GroqStoryboardPlannerCapability(
                                             groqKeyOpt: Option[String] = sys.env.get("GROQ_API_KEY").map(_.trim).filter(_.nonEmpty),
                                             preferredModelOpt: Option[String] = None
                                           ) extends StoryboardPlannerCapability:

  override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
    val cleanStory = story.trim
    if cleanStory.isEmpty then
      break(AIDirectorError("La descrizione della storia non può essere vuota."))

    val groqKey = groqKeyOpt.getOrElse(
      break(AIDirectorError("Variabile d'ambiente GROQ_API_KEY mancante."))
    )

    val modelId = preferredModelOpt.getOrElse(resolveBestGroqModel(groqKey))

    val systemInstruction =
      "Sei un regista cinematografico esperto per un sistema multi-agente. " +
        "Analizza l'intera storia dall'inizio alla fine, dividila in 4 o 5 scene cronologiche distinte " +
        "che coprano tutti gli eventi della trama (introduzione, incontro/sviluppo, azione centrale, finale) " +
        "e invoca il tool dispatch_visual_artists una sola volta passando TUTTE le scene nell'array scenes. " +
        "REGOLA VISIVA FONDAMENTALE: visual_style_bible deve contenere SOLO la tecnica artistica, la palette colori, " +
        "l'illuminazione e l'aspetto base del protagonista (MAI oggetti, luoghi o personaggi che appaiono solo nelle scene successive). " +
        "Ogni singolo visual_prompt deve descrivere ESCLUSIVAMENTE ciò che è fisicamente visibile nell'inquadratura di QUEL preciso momento: " +
        "non menzionare mai eventi passati o futuri (es. non scrivere 'prima di incontrare il drago', perché il generatore di immagini disegnerebbe il drago nella scena 1). " +
        "Scrivi narration in italiano scorrevole e coinvolgente per il doppiaggio."

    val payload =
      s"""{
         |  "model": "${escapeJson(modelId)}",
         |  "temperature": 0.4,
         |  "tool_choice": "required",
         |  "messages": [
         |    {"role": "system", "content": "${escapeJson(systemInstruction)}"},
         |    {"role": "user", "content": "${escapeJson(cleanStory)}"}
         |  ],
         |  "tools": [
         |    {
         |      "type": "function",
         |      "function": {
         |        "name": "dispatch_visual_artists",
         |        "description": "Invia il piano completo delle scene dello storyboard al pool di agenti artisti visivi per generare tutte le immagini in parallelo.",
         |        "parameters": {
         |          "type": "object",
         |          "properties": {
         |            "visual_style_bible": {
         |              "type": "string",
         |              "description": "SOLO stile artistico, illuminazione, resa grafica e aspetto base del protagonista (es. 'Pixar 3D animation style, warm cinematic lighting, young boy with curly brown hair and red hoodie'). VIETATO inserire elementi della trama o personaggi secondari."
         |            },
         |            "scenes": {
         |              "type": "array",
         |              "description": "Elenco cronologico completo delle scene (da 4 a 5 scene che coprono tutta la storia).",
         |              "items": {
         |                "type": "object",
         |                "properties": {
         |                  "scene_number": { "type": "integer", "description": "Numero progressivo della scena da 1 a N." },
         |                  "shot_type": { "type": "string", "description": "Inquadratura (es. EstablishingWide, DynamicMedium, DramaticCloseUp, FinalPanorama)." },
         |                  "narration": { "type": "string", "description": "Frase narrativa in italiano per questa scena." },
         |                  "visual_prompt": { "type": "string", "description": "Prompt visivo in inglese per FLUX.1: descrivi SOLO oggetti, personaggi e ambientazione visibili in questa specifica scena, senza mai nominare elementi delle scene successive o precedenti." }
         |                },
         |                "required": ["scene_number", "shot_type", "narration", "visual_prompt"]
         |              }
         |            }
         |          },
         |          "required": ["visual_style_bible", "scenes"]
         |        }
         |      }
         |    }
         |  ]
         |}""".stripMargin

    val responseBody = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create("https://api.groq.com/openai/v1/chat/completions"))
        .timeout(Duration.ofSeconds(40))
        .header("Authorization", s"Bearer $groqKey")
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIDirectorError(s"Errore Groq (HTTP ${response.statusCode()}): ${response.body().take(250)}"))
    }.getOrElse(break(AIDirectorError("Errore di rete durante la chiamata a Groq.")))

    parseStoryboardFromGroq(responseBody)

  private def resolveBestGroqModel(groqKey: String)(using Label[AIDirectorError]): String =
    val body = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create("https://api.groq.com/openai/v1/models"))
        .timeout(Duration.ofSeconds(20))
        .header("Authorization", s"Bearer $groqKey")
        .GET()
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIDirectorError(s"Impossibile leggere i modelli Groq (HTTP ${response.statusCode()})."))
    }.getOrElse(break(AIDirectorError("Errore di rete nel recupero dei modelli Groq.")))

    val root = MiniJson.parse(body).getOrElse(
      break(AIDirectorError("Risposta JSON non valida dall'endpoint modelli Groq."))
    )

    val ids = for
      obj    <- root.obj.toList
      data   <- obj.get("data").flatMap(_.arr).toList
      item   <- data
      fields <- item.obj.toList
      id     <- fields.get("id").flatMap(_.str).toList
      if GroqStoryboardPlannerCapability.scoreGroqModel(id) > 0
    yield id

    ids.distinct
      .sortBy(id => -GroqStoryboardPlannerCapability.scoreGroqModel(id))
      .headOption
      .getOrElse(break(AIDirectorError("Nessun modello chat disponibile sul tuo account Groq.")))

  private[ai] def parseStoryboardFromGroq(responseBody: String)(using Label[AIDirectorError]): StoryboardPlan =
    val root = MiniJson.parse(responseBody).getOrElse(
      break(AIDirectorError("Risposta JSON Groq non parsabile."))
    )

    val toolCalls = for
      rootObj <- root.obj.toList
      choices <- rootObj.get("choices").flatMap(_.arr).toList
      first   <- choices.headOption.flatMap(_.obj).toList
      msg     <- first.get("message").flatMap(_.obj).toList
      calls   <- msg.get("tool_calls").flatMap(_.arr).toList
      call    <- calls
      callObj <- call.obj.toList
      fnObj   <- callObj.get("function").flatMap(_.obj).toList
      argsStr <- fnObj.get("arguments").flatMap(_.str).toList
    yield argsStr

    val argsJsonStr = toolCalls.headOption.getOrElse(
      break(AIDirectorError(s"Nessuna tool_call trovata nella risposta Groq: ${responseBody.take(300)}"))
    )

    val argsObj = MiniJson.parse(argsJsonStr).toOption.flatMap(_.obj).getOrElse(
      break(AIDirectorError("Argomenti della tool_call dispatch_visual_artists non validi."))
    )

    val styleBible = argsObj.get("visual_style_bible").flatMap(_.str).getOrElse("")
    val rawScenes = argsObj.get("scenes").flatMap(_.arr).getOrElse(Nil)

    val directives = rawScenes.flatMap { item =>
      for
        sObj   <- item.obj
        num    <- sObj.get("scene_number").flatMap(_.double).map(_.toInt)
        shot   <- sObj.get("shot_type").flatMap(_.str)
        narr   <- sObj.get("narration").flatMap(_.str)
        prompt <- sObj.get("visual_prompt").flatMap(_.str)
      yield SceneDirective(num, shot, narr, prompt)
    }.sortBy(_.sceneNumber)

    if directives.isEmpty then
      break(AIDirectorError("Lo storyboard generato non contiene alcuna scena valida."))

    StoryboardPlan(styleBible, directives)

object GroqStoryboardPlannerCapability:
  private[ai] def scoreGroqModel(id: String): Int =
    val lower = id.toLowerCase
    if lower.contains("whisper") || lower.contains("guard") || lower.contains("tts") ||
      lower.contains("orpheus") || lower.contains("playai") || lower.contains("embed") ||
      lower.contains("safeguard") || lower.contains("moderation")
    then -1
    else if lower.contains("llama-4") && lower.contains("maverick") then 100
    else if lower.contains("llama-4") && lower.contains("scout") then 95
    else if lower.contains("llama-4") then 90
    else if lower.contains("llama-3.3") && lower.contains("70b") then 85
    else if lower.contains("qwen") && (lower.contains("32b") || lower.contains("72b") || lower.contains("3")) then 80
    else if lower.contains("gpt-oss") && lower.contains("120b") then 78
    else if lower.contains("gpt-oss") then 75
    else if lower.contains("70b") then 70
    else if lower.contains("llama-3.1") && lower.contains("8b") then 60
    else if lower.contains("llama") then 50
    else 10