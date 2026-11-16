package spike

import java.awt.Desktop
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.Duration
import java.util.Base64
import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.util.Try

final case class SceneDirective(
                                 sceneNumber: Int,
                                 shotType: String,
                                 narration: String,
                                 visualPrompt: String
                               )

final case class StoryboardPlan(
                                 styleBible: String,
                                 scenes: List[SceneDirective]
                               )

@main def runGroqCloudflareSandbox(): Unit =
  given ExecutionContext = ExecutionContext.global

  val config =
    for
      groqKey   <- sys.env.get("GROQ_API_KEY").map(_.trim).filter(_.nonEmpty)
      accountId <- sys.env.get("CF_ACCOUNT_ID").map(_.trim).filter(_.nonEmpty)
      apiToken  <- sys.env.get("CF_API_TOKEN").map(_.trim).filter(_.nonEmpty)
    yield (groqKey, accountId, apiToken)

  config match
    case None =>
      println("Variabili GROQ_API_KEY, CF_ACCOUNT_ID o CF_API_TOKEN mancanti. Verifica setx e riavvia IntelliJ.")
    case Some((groqKey, accountId, apiToken)) =>
      resolveBestGroqModel(groqKey) match
        case Left(err) =>
          println(s"[ERRORE GROQ] $err")
        case Right(modelId) =>
          println(s"Connesso a Groq. Modello selezionato: $modelId")
          val sample = "Un piccolo gatto astronauta arancione atterra su un pianeta di formaggio, incontra un topo robot amichevole e insieme costruiscono un razzo per tornare a casa."
          produceDirectedStory(sample, groqKey, modelId, accountId, apiToken)

          Iterator
            .continually {
              print("\nInserisci una storia (o 'exit' per uscire): ")
              Option(StdIn.readLine())
            }
            .takeWhile(_.exists(_.trim.toLowerCase != "exit"))
            .flatten
            .filter(_.trim.nonEmpty)
            .foreach(produceDirectedStory(_, groqKey, modelId, accountId, apiToken))

private val sharedHttpClient: HttpClient =
  HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

private enum JsonVal:
  case JObj(fields: Map[String, JsonVal])
  case JArr(items: List[JsonVal])
  case JStr(value: String)
  case JNum(value: Double)
  case JBool(value: Boolean)
  case JNull

  def obj: Option[Map[String, JsonVal]] = this match
    case JObj(f) => Some(f)
    case _       => None

  def arr: Option[List[JsonVal]] = this match
    case JArr(xs) => Some(xs)
    case _        => None

  def str: Option[String] = this match
    case JStr(s) => Some(s)
    case _       => None

  def int: Option[Int] = this match
    case JNum(n) => Some(n.toInt)
    case _       => None

private object MiniJson:
  def parse(input: String): Either[String, JsonVal] =
    parseValue(input, 0).map(_._1)

  @tailrec
  private def skipWs(s: String, i: Int): Int =
    if i < s.length && s.charAt(i).isWhitespace then skipWs(s, i + 1) else i

  private def parseValue(s: String, i: Int): Either[String, (JsonVal, Int)] =
    val idx = skipWs(s, i)
    if idx >= s.length then Left("Fine imprevista del JSON")
    else
      s.charAt(idx) match
        case '{' => parseObject(s, idx + 1)
        case '[' => parseArray(s, idx + 1)
        case '"' => parseString(s, idx + 1).map((str, next) => (JsonVal.JStr(str), next))
        case 't' if s.startsWith("true", idx)  => Right((JsonVal.JBool(true), idx + 4))
        case 'f' if s.startsWith("false", idx) => Right((JsonVal.JBool(false), idx + 5))
        case 'n' if s.startsWith("null", idx)  => Right((JsonVal.JNull, idx + 4))
        case c if c == '-' || c.isDigit        => parseNumber(s, idx)
        case other                             => Left(s"Carattere JSON inatteso '$other' a $idx")

  private def parseString(s: String, start: Int): Either[String, (String, Int)] =
    @tailrec
    def loop(idx: Int, acc: StringBuilder): Either[String, (String, Int)] =
      if idx >= s.length then Left("Stringa JSON non terminata")
      else
        s.charAt(idx) match
          case '"' => Right((acc.toString, idx + 1))
          case '\\' =>
            if idx + 1 >= s.length then Left("Escape non terminato")
            else
              s.charAt(idx + 1) match
                case '"'  => loop(idx + 2, acc.append('"'))
                case '\\' => loop(idx + 2, acc.append('\\'))
                case '/'  => loop(idx + 2, acc.append('/'))
                case 'b'  => loop(idx + 2, acc.append('\b'))
                case 'f'  => loop(idx + 2, acc.append('\f'))
                case 'n'  => loop(idx + 2, acc.append('\n'))
                case 'r'  => loop(idx + 2, acc.append('\r'))
                case 't'  => loop(idx + 2, acc.append('\t'))
                case 'u' if idx + 5 < s.length =>
                  val hex = s.substring(idx + 2, idx + 6)
                  val ch = Try(Integer.parseInt(hex, 16).toChar).getOrElse(' ')
                  loop(idx + 6, acc.append(ch))
                case other => loop(idx + 2, acc.append(other))
          case ch => loop(idx + 1, acc.append(ch))
    loop(start, new StringBuilder)

  private def parseNumber(s: String, start: Int): Either[String, (JsonVal, Int)] =
    @tailrec
    def endIdx(i: Int): Int =
      if i < s.length && "-+0123456789.eE".indexOf(s.charAt(i)) >= 0 then endIdx(i + 1) else i
    val end = endIdx(start)
    s.substring(start, end).toDoubleOption match
      case Some(n) => Right((JsonVal.JNum(n), end))
      case None    => Left(s"Numero non valido a $start")

  private def parseArray(s: String, start: Int): Either[String, (JsonVal, Int)] =
    val first = skipWs(s, start)
    if first < s.length && s.charAt(first) == ']' then Right((JsonVal.JArr(Nil), first + 1))
    else
      @tailrec
      def loop(idx: Int, acc: List[JsonVal]): Either[String, (JsonVal, Int)] =
        parseValue(s, idx) match
          case Left(err) => Left(err)
          case Right((v, afterVal)) =>
            val next = skipWs(s, afterVal)
            if next >= s.length then Left("Array non terminato")
            else
              s.charAt(next) match
                case ',' => loop(next + 1, v :: acc)
                case ']' => Right((JsonVal.JArr((v :: acc).reverse), next + 1))
                case c   => Left(s"Atteso ',' o ']' ma trovato '$c'")
      loop(first, Nil)

  private def parseObject(s: String, start: Int): Either[String, (JsonVal, Int)] =
    val first = skipWs(s, start)
    if first < s.length && s.charAt(first) == '}' then Right((JsonVal.JObj(Map.empty), first + 1))
    else
      @tailrec
      def loop(idx: Int, acc: Map[String, JsonVal]): Either[String, (JsonVal, Int)] =
        val keyStart = skipWs(s, idx)
        if keyStart >= s.length || s.charAt(keyStart) != '"' then Left("Attesa chiave stringa")
        else
          parseString(s, keyStart + 1) match
            case Left(err) => Left(err)
            case Right((key, afterKey)) =>
              val colon = skipWs(s, afterKey)
              if colon >= s.length || s.charAt(colon) != ':' then Left("Atteso ':'")
              else
                parseValue(s, colon + 1) match
                  case Left(err) => Left(err)
                  case Right((v, afterVal)) =>
                    val next = skipWs(s, afterVal)
                    if next >= s.length then Left("Oggetto non terminato")
                    else
                      s.charAt(next) match
                        case ',' => loop(next + 1, acc + (key -> v))
                        case '}' => Right((JsonVal.JObj(acc + (key -> v)), next + 1))
                        case c   => Left(s"Atteso ',' o '}' ma trovato '$c'")
      loop(first, Map.empty)

private def scoreGroqModel(id: String): Int =
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

private def resolveBestGroqModel(groqKey: String): Either[String, String] =
  Try {
    val request = HttpRequest.newBuilder()
      .uri(URI.create("https://api.groq.com/openai/v1/models"))
      .timeout(Duration.ofSeconds(20))
      .header("Authorization", s"Bearer $groqKey")
      .GET()
      .build()

    val response = sharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    if response.statusCode() == 200 then
      MiniJson.parse(response.body()).flatMap { root =>
        val ids = for
          obj   <- root.obj.toList
          data  <- obj.get("data").flatMap(_.arr).toList
          item  <- data
          fields <- item.obj.toList
          id    <- fields.get("id").flatMap(_.str).toList
          if scoreGroqModel(id) > 0
        yield id

        ids.distinct.sortBy(id => -scoreGroqModel(id)).headOption.toRight(
          "Nessun modello chat disponibile sul tuo account Groq."
        )
      }
    else
      Left(s"Impossibile leggere i modelli Groq (HTTP ${response.statusCode()}): ${response.body()}")
  }.toEither.left.map(ex => s"Errore di rete verso Groq: ${ex.getMessage}").flatten

private def escapeJson(text: String): String =
  text
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", " ")
    .replace("\r", " ")
    .replace("\t", " ")

private def parseStoryboardFromGroq(responseBody: String): Either[String, StoryboardPlan] =
  MiniJson.parse(responseBody).flatMap { root =>
    val toolCalls = for
      rootObj   <- root.obj.toList
      choices   <- rootObj.get("choices").flatMap(_.arr).toList
      first     <- choices.headOption.flatMap(_.obj).toList
      msg       <- first.get("message").flatMap(_.obj).toList
      calls     <- msg.get("tool_calls").flatMap(_.arr).toList
      call      <- calls
      callObj   <- call.obj.toList
      fnObj     <- callObj.get("function").flatMap(_.obj).toList
      argsStr   <- fnObj.get("arguments").flatMap(_.str).toList
    yield argsStr

    toolCalls.headOption match
      case None =>
        Left(s"Nessuna tool_call trovata nella risposta Groq: ${responseBody.take(400)}")
      case Some(argsJsonStr) =>
        MiniJson.parse(argsJsonStr).flatMap { argsVal =>
          argsVal.obj match
            case None => Left("Argomenti del tool non validi")
            case Some(argsObj) =>
              val styleBible = argsObj.get("visual_style_bible").flatMap(_.str).getOrElse("")
              val rawScenes = argsObj.get("scenes").flatMap(_.arr).getOrElse(Nil)
              val directives = rawScenes.flatMap { item =>
                for
                  sObj   <- item.obj
                  num    <- sObj.get("scene_number").flatMap(_.int)
                  shot   <- sObj.get("shot_type").flatMap(_.str)
                  narr   <- sObj.get("narration").flatMap(_.str)
                  prompt <- sObj.get("visual_prompt").flatMap(_.str)
                yield SceneDirective(num, shot, narr, prompt)
              }.sortBy(_.sceneNumber)

              if directives.nonEmpty then Right(StoryboardPlan(styleBible, directives))
              else Left(s"Array scenes vuoto nella tool_call: ${argsJsonStr.take(400)}")
        }
  }

private def callDirectorAgent(
                               story: String,
                               groqKey: String,
                               modelId: String
                             )(using ExecutionContext): Future[Either[String, StoryboardPlan]] =
  Future {
    val systemInstruction =
      "Sei un regista cinematografico esperto per un sistema multi-agente. " +
        "Analizza l'intera storia dall'inizio alla fine, dividila in 4 o 5 scene distinte " +
        "che coprano tutti gli eventi della trama (introduzione, incontro/sviluppo, azione centrale, finale) " +
        "e invoca il tool dispatch_visual_artists una sola volta passando TUTTE le scene nell'array scenes. " +
        "Definisci prima visual_style_bible in inglese (aspetto esatto dei personaggi, colori, stile 3D/illustrazione) " +
        "e ripeti quegli stessi dettagli visivi in inglese dentro ogni singolo visual_prompt. " +
        "Scrivi narration in italiano scorrevole per il doppiaggio."

    val payload =
      s"""{
         |  "model": "${escapeJson(modelId)}",
         |  "temperature": 0.4,
         |  "tool_choice": "required",
         |  "messages": [
         |    {"role": "system", "content": "${escapeJson(systemInstruction)}"},
         |    {"role": "user", "content": "${escapeJson(story)}"}
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
         |              "description": "Scheda visiva fissa in inglese con aspetto dei personaggi, vestiti e stile grafico."
         |            },
         |            "scenes": {
         |              "type": "array",
         |              "description": "Elenco cronologico completo delle scene (da 4 a 5 scene che coprono tutta la storia).",
         |              "items": {
         |                "type": "object",
         |                "properties": {
         |                  "scene_number": {
         |                    "type": "integer",
         |                    "description": "Numero progressivo della scena da 1 a N."
         |                  },
         |                  "shot_type": {
         |                    "type": "string",
         |                    "description": "Inquadratura (es. EstablishingWide, DynamicMedium, DramaticCloseUp, FinalPanorama)."
         |                  },
         |                  "narration": {
         |                    "type": "string",
         |                    "description": "Frase narrativa in italiano per questa scena."
         |                  },
         |                  "visual_prompt": {
         |                    "type": "string",
         |                    "description": "Prompt visivo in inglese per FLUX.1 con stile, personaggi coerenti e azione della scena."
         |                  }
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

    val request = HttpRequest.newBuilder()
      .uri(URI.create("https://api.groq.com/openai/v1/chat/completions"))
      .timeout(Duration.ofSeconds(40))
      .header("Authorization", s"Bearer $groqKey")
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
      .build()

    val response = sharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    if response.statusCode() == 200 then parseStoryboardFromGroq(response.body())
    else Left(s"Errore Groq (HTTP ${response.statusCode()}): ${response.body()}")
  }.recover { case ex =>
    Left(s"Errore di rete verso Groq: ${ex.getMessage}")
  }

private def detectImageExtension(bytes: Array[Byte]): String =
  if bytes.length >= 4 &&
    bytes(0) == 0x89.toByte &&
    bytes(1) == 0x50.toByte &&
    bytes(2) == 0x4E.toByte &&
    bytes(3) == 0x47.toByte
  then "png"
  else "jpg"

private def callVisualArtistAgent(
                                   accountId: String,
                                   apiToken: String,
                                   outputDir: Path,
                                   directive: SceneDirective
                                 )(using ExecutionContext): Future[Either[String, Path]] =
  Future {
    val endpoint = s"https://api.cloudflare.com/client/v4/accounts/$accountId/ai/run/@cf/black-forest-labs/flux-1-schnell"
    val payload = s"""{"prompt":"${escapeJson(directive.visualPrompt)}","steps":4}"""
    val request = HttpRequest.newBuilder()
      .uri(URI.create(endpoint))
      .timeout(Duration.ofSeconds(60))
      .header("Authorization", s"Bearer $apiToken")
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
      .build()

    val response = sharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    if response.statusCode() == 200 then
      val base64Opt = for
        root   <- MiniJson.parse(response.body()).toOption
        rObj   <- root.obj
        resObj <- rObj.get("result").flatMap(_.obj)
        imgStr <- resObj.get("image").flatMap(_.str)
      yield imgStr

      base64Opt match
        case Some(b64) =>
          val bytes = Base64.getDecoder.decode(b64)
          val ext = detectImageExtension(bytes)
          val safeShot = directive.shotType.replaceAll("[^a-zA-Z0-9_-]", "")
          val targetPath = outputDir.resolve(s"scene_${directive.sceneNumber}_$safeShot.$ext")
          Files.write(targetPath, bytes)
          Right(targetPath)
        case None =>
          Left(s"Scena ${directive.sceneNumber} senza campo image: ${response.body().take(250)}")
    else
      Left(s"Scena ${directive.sceneNumber} fallita (HTTP ${response.statusCode()}): ${response.body()}")
  }.recover { case ex =>
    Left(s"Scena ${directive.sceneNumber} errore di rete: ${ex.getMessage}")
  }

private def produceDirectedStory(
                                  story: String,
                                  groqKey: String,
                                  modelId: String,
                                  accountId: String,
                                  apiToken: String
                                )(using ExecutionContext): Unit =
  println(s"\nStoria: \"$story\"")
  println("Regista Groq in azione (creazione storyboard completo)...")

  Await.result(callDirectorAgent(story, groqKey, modelId), 45.seconds) match
    case Left(err) =>
      println(s"[ERRORE REGISTA] $err")
    case Right(plan) =>
      val outputDir = Paths.get("sandbox_groq_cf_output", System.currentTimeMillis().toString)
      Files.createDirectories(outputDir)

      println(s"Style Bible: ${plan.styleBible}")
      println(s"Storyboard approvato (${plan.scenes.size} scene). Generazione parallela su Cloudflare...")
      plan.scenes.foreach { d =>
        println(s"  ${d.sceneNumber}. [${d.shotType}] Voce: \"${d.narration}\"")
        println(s"     Prompt: ${d.visualPrompt}")
      }

      val tasks = plan.scenes.map(callVisualArtistAgent(accountId, apiToken, outputDir, _))
      val results = Await.result(Future.sequence(tasks), 90.seconds)

      results.foreach {
        case Right(path) => println(s"  [OK] ${path.toAbsolutePath}")
        case Left(err)   => println(s"  [ERRORE] $err")
      }

      if results.exists(_.isRight) then
        Try(Desktop.getDesktop.open(outputDir.toFile))