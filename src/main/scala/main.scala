import domain.{TicketConfig, OfficeState, RouteTariff, Train}
import monads.{*, given}
import plan.*
import ui.{MenuLeaf, MenuTreeNode}

@main def main(): Unit =
  // Конфигурация кассы: тарифы, багаж, штраф за возврат
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

  // Создание карты мест: rows рядов по 4 места (A, B, C, D)
  def makeSeats(rows: Int): Map[String, Boolean] =
    (1 to rows).flatMap(r => Seq("A","B","C","D").map(c => s"$r$c" -> false)).toMap

  // Начальное состояние: два поезда по 12 мест каждый
  val initState = OfficeState(
    trains = List(
      Train("Express-1", "Moscow-SPb",   makeSeats(3)),
      Train("Express-2", "Moscow-Kazan", makeSeats(3))
    ),
    soldTickets  = List.empty,
    revenue      = 0.0,
    nextTicketId = 1
  )

  // Построение меню
  val root = MenuTreeNode(
    "Железнодорожная касса",
    Seq(
      MenuLeaf("Показать поезда",         showTrainsAction),
      MenuLeaf("Показать билеты",         showTicketsAction),
      MenuLeaf("Купить билет",            bookTicketAction),
      MenuLeaf("Вернуть билет",           cancelTicketAction),
      MenuLeaf("Добавить поезд",          addTrainAction),
      MenuLeaf("Следующий день",          nextDayAction),
      MenuLeaf("Демонстрация Writer",     writerDemoAction)
    )
  )

  val program =
    for
      _ <- IO.writeLine("Железнодорожная касса (Монады)")
      _ <- IO.writeLine(s"Маршруты: ${cfg.tariffs.keys.mkString(", ")}")
      _ <- IO.writeLine(s"Багаж: ${cfg.baggagePerKg} за кг, штраф за возврат: ${cfg.refundPenaltyPercent * 100}%")
      _ <- root.userInteractionLoop(initState, cfg)
      _ <- IO.writeLine("пока")
    yield ()

  program.unsafeRun()