import domain.{TicketConfig, OfficeState, RouteTariff, Train, Storage}
import monads.{*, given}
import plan.*
import ui.{MenuLeaf, MenuTreeNode}

@main def main(): Unit =
  val cfg = TicketConfig(
    tariffs = Map(
      "Moscow-SPb"   -> RouteTariff("Moscow-SPb",   2500, 5000),
      "Moscow-Kazan" -> RouteTariff("Moscow-Kazan", 1800, 3600),
      "SPb-Sochi"    -> RouteTariff("SPb-Sochi",    3200, 6400)
    ),
    baggagePerKg         = 50.0,
    seatRule             = "any",
    refundPenaltyPercent = 0.15
  )

  def makeSeats(rows: Int): Map[String, Boolean] =
    (1 to rows).flatMap(r => Seq("A", "B", "C", "D").map(c => s"$r$c" -> false)).toMap

  val defaultState = OfficeState(
    trains = List(
      Train("Express-1", "Moscow-SPb",   makeSeats(3)),
      Train("Express-2", "Moscow-Kazan", makeSeats(3))
    ),
    soldTickets  = List.empty,
    revenue      = 0.0,
    nextTicketId = 1
  )

  // загружаем сохранённое состояние или используем начальное
  val initState = Storage.load().getOrElse(defaultState)

  val root = MenuTreeNode(
    "Железнодорожная касса",
    Seq(
      MenuLeaf("Показать поезда",     showTrainsAction),
      MenuLeaf("Показать билеты",     showTicketsAction),
      MenuLeaf("Купить билет",        bookTicketAction),
      MenuLeaf("Вернуть билет",       cancelTicketAction),
      MenuLeaf("Добавить поезд",      addTrainAction),
      MenuLeaf("Следующий день",      nextDayAction),
      MenuLeaf("Writer demo",         writerDemoAction),
      MenuLeaf("Удалить поезд",       removeTrainAction)
    )
  )

  val program =
    for
      _ <- IO.writeLine("=== Железнодорожная касса (монады) ===")
      _ <- IO.writeLine(s"Маршруты: ${cfg.tariffs.keys.mkString(", ")}")
      _ <- IO.writeLine(s"Багаж: ${cfg.baggagePerKg} руб/кг, штраф за возврат: ${cfg.refundPenaltyPercent * 100}%")
      finalState <- root.userInteractionLoop(initState, cfg)
      _          <- IO.delay(Storage.save(finalState))  // сохраняем при выходе
      _          <- IO.writeLine("пока")
    yield ()

  program.unsafeRun()
