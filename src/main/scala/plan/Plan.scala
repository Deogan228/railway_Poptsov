package plan

import monads.{*, given}
import domain.{OfficeState, TicketConfig, ClassType, Train, FuncState, FuncWriter}

// Convert State action to IO
def runState[A](s: OfficeState, action: State[OfficeState, A]): IO[(OfficeState, A)] =
  IO.delay(action.run(s))

// Print accumulated log
def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(()))((acc, line) => acc.flatMap(_ => IO.writeLine("  " + line)))

// Read an integer with prompt and fallback
def readInt(prompt: String, fallback: Int): IO[Int] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toIntOption.getOrElse(fallback)

// Read a double with prompt and fallback
def readDouble(prompt: String, fallback: Double): IO[Double] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toDoubleOption.getOrElse(fallback)

// Read a string with prompt
def readStr(prompt: String): IO[String] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim

// Show available trains
def showTrainsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.trains.zipWithIndex.map { case (t, i) =>
    val free = t.seats.count(!_._2)
    val total = t.seats.size
    s"  ${i + 1}. ${t.name} | route: ${t.route} | seats: $free/$total free"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  No trains available" else info).map(_ => s)

// Show sold tickets
def showTicketsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.soldTickets.map { t =>
    s"  #${t.id} ${t.route} ${t.classType} seat=${t.seat} price=${t.price} baggage=${t.baggageCost}"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  No tickets sold" else info).map(_ => s)

// Book a ticket interactively
def bookTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTrainsAction(s, cfg)
    trainName <- readStr("Train name: ")
    seat      <- readStr("Seat (e.g. 1A): ")
    clsStr    <- readStr("Class (economy/business): ")
    cls        = if clsStr.toLowerCase.startsWith("b") then ClassType.Business else ClassType.Economy
    baggage   <- readDouble("Baggage weight kg (0 for none): ", 0.0)
    res       <- runState(s, FuncState.bookTicket(trainName, seat, cls, baggage)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Cancel a ticket interactively
def cancelTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTicketsAction(s, cfg)
    ticketId  <- readInt("Ticket id to cancel: ", -1)
    res       <- runState(s, FuncState.cancelTicket(ticketId)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Add a train interactively
def addTrainAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    name    <- readStr("Train name: ")
    route   <- readStr("Route (e.g. Moscow-SPb): ")
    nSeats  <- readInt("Number of seats: ", 10)
    seats    = (1 to nSeats).flatMap { row =>
                 Seq("A", "B", "C", "D").map(col => s"$row$col" -> false)
               }.toMap
    train    = Train(name, route, seats)
    res     <- runState(s, FuncState.addTrain(train))
    (ns, log) = res
    _       <- printLog(log)
  yield ns

// Advance to next day
def nextDayAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    res       <- runState(s, FuncState.nextDay)
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Pure Writer demonstration
def writerDemoAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val w1 = FuncWriter.logRouteChoice("Moscow-SPb")
  val w2 = FuncWriter.logPriceCalc("Moscow-SPb", 3500, 200)
  val w3 = FuncWriter.logSeatAssigned("2A", "Train 42")
  val w4 = FuncWriter.logRefund(1, 3150)
  val allLogs = w1.log ++ w2.log ++ w3.log ++ w4.log
  IO.writeLine("[Writer demo]").flatMap(_ =>
    allLogs.foldLeft(IO.pure(()))((io, line) => io.flatMap(_ => IO.writeLine("  " + line)))
  ).map(_ => s)
