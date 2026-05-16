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

    // 1. Successful booking
    val (s1, log1) = FuncState.bookTicket("T1", "1A", ClassType.Economy, 10.0)(cfg).run(s0)
    assert(s1.soldTickets.size == 1, "Ticket must be added")
    assert(s1.soldTickets.head.price == 2500, "Price must match economy tariff")
    assert(s1.soldTickets.head.baggageCost == 500, "Baggage cost = 10 * 50")
    assert(s1.revenue == 3000, "Revenue = 2500 + 500")
    assert(log1.exists(_.contains("booked")), "Log must contain booking")

    // 2. Occupied seat
    val (s2, log2) = FuncState.bookTicket("T1", "2B", ClassType.Economy, 0)(cfg).run(s0)
    assert(s2.soldTickets.isEmpty, "No ticket for occupied seat")
    assert(log2.exists(_.contains("not available")), "Log must note unavailability")

    // 3. Unknown train
    val (s3, log3) = FuncState.bookTicket("T999", "1A", ClassType.Economy, 0)(cfg).run(s0)
    assert(s3.soldTickets.isEmpty, "No ticket for unknown train")
    assert(log3.exists(_.contains("not found")), "Log must note train not found")

    // 4. Cancel ticket with refund
    val ticket = Ticket(1, "Moscow-SPb", ClassType.Economy, "1A", 2500, 10.0, 500)
    val sWithTicket = OfficeState(List(train.copy(seats = seats.updated("1A", true))),
                                  List(ticket), 3000.0, 2)
    val (s4, log4) = FuncState.cancelTicket(1)(cfg).run(sWithTicket)
    assert(s4.soldTickets.isEmpty, "Ticket must be removed")
    val expectedRefund = 3000 * 0.85   // 2550
    assert(math.abs(s4.revenue - (3000 - expectedRefund)) < 0.01, s"Revenue after refund: ${s4.revenue}")
    assert(log4.exists(_.contains("cancelled")), "Log must note cancellation")

    // 5. Reader: ticketPrice
    val priceOpt = FuncReader.ticketPrice("Moscow-SPb", ClassType.Business).run(cfg)
    assert(priceOpt.contains(5000), "Business price = 5000")

    // 6. Reader: baggageCost
    val bc = FuncReader.baggageCost(5).run(cfg)
    assert(bc == 250, "Baggage cost = 5 * 50")

    // 7. Reader: seatAvailable
    assert(FuncReader.seatAvailable(train, "1A").run(cfg), "1A must be free")
    assert(!FuncReader.seatAvailable(train, "2B").run(cfg), "2B must be occupied")

    // 8. Reader: refundAmount
    val refund = FuncReader.refundAmount(ticket).run(cfg)
    assert(math.abs(refund - 2550) < 0.01, s"Refund = 3000 * 0.85 = 2550, got $refund")

    // 9. addTrain
    val newTrain = Train("T2", "Moscow-SPb", Map("1A" -> false))
    val (s5, _) = FuncState.addTrain(newTrain).run(s0)
    assert(s5.trains.size == 2, "Train must be added")

    // 10. nextDay
    val (s6, _) = FuncState.nextDay.run(sWithTicket)
    assert(s6.soldTickets.isEmpty, "Tickets cleared on next day")
    assert(s6.revenue == 0.0, "Revenue reset on next day")

    println("All tests passed")
