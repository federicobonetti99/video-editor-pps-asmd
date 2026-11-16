package app.ai.capabilities.http

import java.net.http.HttpClient
import java.time.Duration
import scala.annotation.tailrec
import scala.util.Try

private[http] val SharedHttpClient: HttpClient =
  HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

private[http] def escapeJson(text: String): String =
  text
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", " ")
    .replace("\r", " ")
    .replace("\t", " ")

private[http] enum JsonVal:
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

  def double: Option[Double] = this match
    case JNum(n) => Some(n)
    case _       => None

private[http] object MiniJson:
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