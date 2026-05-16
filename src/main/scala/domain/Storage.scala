package domain

import java.io.{File, PrintWriter}
import scala.io.Source

object Storage:
  private val file = "state.json"

  def save(s: OfficeState): Unit =
    val tickets = s.soldTickets.map { t =>
      s"""{"id":${t.id},"route":"${t.route}","class":"${t.classType}","seat":"${t.seat}","price":${t.price},"baggageWeight":${t.baggageWeight},"baggageCost":${t.baggageCost}}"""
    }.mkString("[", ",", "]")

    val trains = s.trains.map { t =>
      val seats = t.seats.map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")
      s"""{"name":"${t.name}","route":"${t.route}","seats":$seats}"""
    }.mkString("[", ",", "]")

    val json = s"""{"revenue":${s.revenue},"nextTicketId":${s.nextTicketId},"trains":$trains,"tickets":$tickets}"""
    val pw = PrintWriter(file)
    pw.write(json)
    pw.close()

  def load(): Option[OfficeState] =
    try
      val raw = Source.fromFile(file).mkString
      Some(parse(raw))
    catch case _ => None

  private def parseString(s: String, key: String): String =
    val pattern = s""""$key":"([^"]*)"""".r
    pattern.findFirstMatchIn(s).map(_.group(1)).getOrElse("")

  private def parseDouble(s: String, key: String): Double =
    val pattern = s""""$key":([\\d.]+)""".r
    pattern.findFirstMatchIn(s).map(_.group(1).toDouble).getOrElse(0.0)

  private def parseInt(s: String, key: String): Int =
    val pattern = s""""$key":(\\d+)""".r
    pattern.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)

  private def parseBool(s: String, key: String): Boolean =
    val pattern = s""""$key":(true|false)""".r
    pattern.findFirstMatchIn(s).map(_.group(1) == "true").getOrElse(false)

  private def parseTickets(json: String): List[Ticket] =
    val block = json.split(""""tickets":""").lift(1).getOrElse("[]")
    val items = block.trim.stripPrefix("[").stripSuffix("}").split("""(?<=\}),(?=\{)""")
    items.filter(_.trim.nonEmpty).flatMap { item =>
      try
        val cls = parseString(item, "class") match
          case "Business" => ClassType.Business
          case _          => ClassType.Economy
        Some(Ticket(
          id            = parseInt(item, "id"),
          route         = parseString(item, "route"),
          classType     = cls,
          seat          = parseString(item, "seat"),
          price         = parseDouble(item, "price"),
          baggageWeight = parseDouble(item, "baggageWeight"),
          baggageCost   = parseDouble(item, "baggageCost")
        ))
      catch case _ => None
    }.toList

  private def parseTrains(json: String): List[Train] =
    val block = json.split(""""trains":""").lift(1).getOrElse("[]")
    val items = block.trim.stripPrefix("[").split("""(?<=\}),(?=\{)""")
    items.filter(_.trim.nonEmpty).flatMap { item =>
      try
        val name  = parseString(item, "name")
        val route = parseString(item, "route")
        val seatsStart = item.indexOf(""""seats":{""") + 9
        val seatsEnd   = item.lastIndexOf("}")
        val seatsStr   = item.substring(seatsStart, seatsEnd)
        val seats = seatsStr.split(",").flatMap { pair =>
          val parts = pair.trim.stripPrefix("{").split(":")
          if parts.length == 2 then
            val k = parts(0).trim.stripPrefix("\"").stripSuffix("\"")
            val v = parts(1).trim == "true"
            Some(k -> v)
          else None
        }.toMap
        Some(Train(name, route, seats))
      catch case _ => None
    }.toList

  private def parse(json: String): OfficeState =
    OfficeState(
      trains       = parseTrains(json),
      soldTickets  = parseTickets(json),
      revenue      = parseDouble(json, "revenue"),
      nextTicketId = parseInt(json, "nextTicketId")
    )