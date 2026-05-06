import domain.{TicketConfig, OfficeState, RouteTariff, Train, Ticket, ClassType, FuncState, FuncReader}
import monads.State

object RailwayTests:
  def main(args: Array[String]): Unit =
    // Конфигурация для тестов: один маршрут, багаж 50 за кг, штраф 15%
    val cfg = TicketConfig(
      tariffs = Map(
        "Moscow-SPb" -> RouteTariff("Moscow-SPb", 2500, 5000)
      ),
      baggagePerKg         = 50.0,
      seatRule             = "any",
      refundPenaltyPercent = 0.15
    )

    val seats = Map("1A" -> false, "1B" -> false, "2A" -> false, "2B" -> true)
    val train = Train("T1", "Moscow-SPb", seats)
    val s0 = OfficeState(List(train), List.empty, 0.0, 1)

    // 1. Успешное бронирование
    val (s1, log1) = FuncState.bookTicket("T1", "1A", ClassType.Economy, 10.0)(cfg).run(s0)
    assert(s1.soldTickets.size == 1, "Билет должен быть добавлен")
    assert(s1.soldTickets.head.price == 2500, "Цена должна соответствовать тарифу эконом")
    assert(s1.soldTickets.head.baggageCost == 500, "Стоимость багажа = 10 * 50")
    assert(s1.revenue == 3000, "Выручка = 2500 + 500")
    assert(log1.exists(_.contains("оформлен")), "Лог должен содержать запись о бронировании")

    // 2. Занятое место — отказ
    val (s2, log2) = FuncState.bookTicket("T1", "2B", ClassType.Economy, 0)(cfg).run(s0)
    assert(s2.soldTickets.isEmpty, "Билет на занятое место не должен быть создан")
    assert(log2.exists(_.contains("занято")), "Лог должен содержать отказ по месту")

    // 3. Несуществующий поезд — отказ
    val (s3, log3) = FuncState.bookTicket("T999", "1A", ClassType.Economy, 0)(cfg).run(s0)
    assert(s3.soldTickets.isEmpty, "Билет на несуществующий поезд не создаётся")
    assert(log3.exists(_.contains("не найден")), "Лог должен содержать ошибку поиска поезда")

    // 4. Возврат билета со штрафом
    val ticket = Ticket(1, "Moscow-SPb", ClassType.Economy, "1A", 2500, 10.0, 500)
    val sWithTicket = OfficeState(List(train.copy(seats = seats.updated("1A", true))),
                                  List(ticket), 3000.0, 2)
    val (s4, log4) = FuncState.cancelTicket(1)(cfg).run(sWithTicket)
    assert(s4.soldTickets.isEmpty, "Билет должен быть удалён")
    val expectedRefund = 3000 * 0.85   // 2550
    assert(math.abs(s4.revenue - (3000 - expectedRefund)) < 0.01, s"Выручка после возврата: ${s4.revenue}")
    assert(log4.exists(_.contains("отменён")), "Лог должен содержать запись об отмене")

    // 5. Reader: стоимость билета бизнес-класса
    val priceOpt = FuncReader.ticketPrice("Moscow-SPb", ClassType.Business).run(cfg)
    assert(priceOpt.contains(5000), "Цена бизнес-класса = 5000")

    // 6. Reader: стоимость багажа
    val bc = FuncReader.baggageCost(5).run(cfg)
    assert(bc == 250, "Стоимость багажа = 5 * 50")

    // 7. Reader: доступность места
    assert(FuncReader.seatAvailable(train, "1A").run(cfg), "Место 1A должно быть свободно")
    assert(!FuncReader.seatAvailable(train, "2B").run(cfg), "Место 2B должно быть занято")

    // 8. Reader: сумма возврата
    val refund = FuncReader.refundAmount(ticket).run(cfg)
    assert(math.abs(refund - 2550) < 0.01, s"Возврат = 3000 * 0.85 = 2550, получено $refund")

    // 9. Добавление поезда
    val newTrain = Train("T2", "Moscow-SPb", Map("1A" -> false))
    val (s5, _) = FuncState.addTrain(newTrain).run(s0)
    assert(s5.trains.size == 2, "Поезд должен быть добавлен")

    // 10. Переход к следующему дню
    val (s6, _) = FuncState.nextDay.run(sWithTicket)
    assert(s6.soldTickets.isEmpty, "Билеты должны быть очищены")
    assert(s6.revenue == 0.0, "Выручка должна быть сброшена")

    println("Все тесты пройдены")