import domain.{TicketConfig, OfficeState, RouteTariff, Train}
import monads.{*, given}
import plan.*
import ui.{MenuLeaf, MenuTreeNode}

@main def main(): Unit =
  val cfg = TicketConfig(
    tariffs = Map(
      "Moscow-SPb"     -> RouteTariff("Moscow-SPb",     2500, 5000),
      "Moscow-Kazan"   -> RouteTariff("Moscow-Kazan",   1800, 3600),
      "SPb-Sochi"      -> RouteTariff("SPb-Sochi",      3200, 6400)
    ),
    baggagePerKg         = 50.0,
    seatRule             = "any",
    refundPenaltyPercent = 0.15
  )

  // Pre-create two trains with 3 rows x 4 columns = 12 seats each
  def makeSeats(rows: Int): Map[String, Boolean] =
    (1 to rows).flatMap(r => Seq("A","B","C","D").map(c => s"$r$c" -> false)).toMap

  val initState = OfficeState(
    trains = List(
      Train("Express-1", "Moscow-SPb",   makeSeats(3)),
      Train("Express-2", "Moscow-Kazan", makeSeats(3))
    ),
    soldTickets  = List.empty,
    revenue      = 0.0,
    nextTicketId = 1
  )

  val root = MenuTreeNode(
    "Railway Ticket Office",
    Seq(
      MenuLeaf("Show trains",       showTrainsAction),
      MenuLeaf("Show tickets",      showTicketsAction),
      MenuLeaf("Book ticket",       bookTicketAction),
      MenuLeaf("Cancel ticket",     cancelTicketAction),
      MenuLeaf("Add train",         addTrainAction),
      MenuLeaf("Next day",          nextDayAction),
      MenuLeaf("Writer demo",       writerDemoAction)
    )
  )

  val program =
    for
      _ <- IO.writeLine("Railway Ticket Office (Monads)")
      _ <- IO.writeLine(s"Routes: ${cfg.tariffs.keys.mkString(", ")}")
      _ <- IO.writeLine(s"Baggage: ${cfg.baggagePerKg} per kg, refund penalty: ${cfg.refundPenaltyPercent * 100}%")
      _ <- root.userInteractionLoop(initState, cfg)
      _ <- IO.writeLine("bye")
    yield ()

  program.unsafeRun()
