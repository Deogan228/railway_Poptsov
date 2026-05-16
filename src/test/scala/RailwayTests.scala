import domain.{TicketConfig, OfficeState, RouteTariff, Train, Ticket, ClassType, FuncState, FuncReader}
import monads.State

object RailwayTests:
  def main(args: Array[String]): Unit =
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

    // 1. успешная покупка
    val (s1, log1) = FuncState.bookTicket("T1", "1A", ClassType.Economy, 10.0)(cfg).run(s0)
    assert(s1.soldTickets.size == 1, "билет должен добавиться")
    assert(s1.soldTickets.head.price == 2500, "цена должна совпасть с тарифом эконома")
    assert(s1.soldTickets.head.baggageCost == 500, "багаж = 10 * 50")
    assert(s1.revenue == 3000, "выручка = 2500 + 500")
    assert(log1.exists(_.contains("оформлен")), "в логе должна быть запись об оформлении")

    // 2. место занято
    val (s2, log2) = FuncState.bookTicket("T1", "2B", ClassType.Economy, 0)(cfg).run(s0)
    assert(s2.soldTickets.isEmpty, "билет не должен оформиться на занятое место")
    assert(log2.exists(_.contains("занято")), "лог про занятое место")

    // 3. поезда нет
    val (s3, log3) = FuncState.bookTicket("T999", "1A", ClassType.Economy, 0)(cfg).run(s0)
    assert(s3.soldTickets.isEmpty, "на несуществующий поезд билета быть не должно")
    assert(log3.exists(_.contains("не найден")), "лог про отсутствие поезда")

    // 4. возврат
    val ticket = Ticket(1, "Moscow-SPb", ClassType.Economy, "1A", 2500, 10.0, 500)
    val sWithTicket = OfficeState(List(train.copy(seats = seats.updated("1A", true))),
                                  List(ticket), 3000.0, 2)
    val (s4, log4) = FuncState.cancelTicket(1)(cfg).run(sWithTicket)
    assert(s4.soldTickets.isEmpty, "билет должен удалиться")
    val expectedRefund = 3000 * 0.85   // 2550
    assert(math.abs(s4.revenue - (3000 - expectedRefund)) < 0.01, s"выручка после возврата: ${s4.revenue}")
    assert(log4.exists(_.contains("отменён")), "лог про отмену")

    // 5. Reader: цена билета
    val priceOpt = FuncReader.ticketPrice("Moscow-SPb", ClassType.Business).run(cfg)
    assert(priceOpt.contains(5000), "бизнес = 5000")

    // 6. Reader: багаж
    val bc = FuncReader.baggageCost(5).run(cfg)
    assert(bc == 250, "багаж = 5 * 50")

    // 7. Reader: место свободно
    assert(FuncReader.seatAvailable(train, "1A").run(cfg), "1A свободно")
    assert(!FuncReader.seatAvailable(train, "2B").run(cfg), "2B занято")

    // 8. Reader: сумма возврата
    val refund = FuncReader.refundAmount(ticket).run(cfg)
    assert(math.abs(refund - 2550) < 0.01, s"возврат = 3000 * 0.85 = 2550, получили $refund")

    // 9. добавление поезда
    val newTrain = Train("T2", "Moscow-SPb", Map("1A" -> false))
    val (s5, _) = FuncState.addTrain(newTrain).run(s0)
    assert(s5.trains.size == 2, "поезд должен добавиться")

    // 10. новый день
    val (s6, _) = FuncState.nextDay.run(sWithTicket)
    assert(s6.soldTickets.isEmpty, "билеты обнулились")
    assert(s6.revenue == 0.0, "выручка обнулилась")

    println("все тесты пройдены")