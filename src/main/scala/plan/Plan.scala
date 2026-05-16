package plan

import monads.{*, given}
import domain.{OfficeState, TicketConfig, ClassType, Train, FuncState, FuncWriter}

// запустить State-действие внутри IO
def runState[A](s: OfficeState, action: State[OfficeState, A]): IO[(OfficeState, A)] =
  IO.delay(action.run(s))

def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(()))((acc, line) => acc.flatMap(_ => IO.writeLine("  " + line)))

def readInt(prompt: String, fallback: Int): IO[Int] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toIntOption.getOrElse(fallback)

def readDouble(prompt: String, fallback: Double): IO[Double] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toDoubleOption.getOrElse(fallback)

def readStr(prompt: String): IO[String] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim

def showTrainsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.trains.zipWithIndex.map { case (t, i) =>
    val free = t.seats.count(!_._2)
    s"  ${i + 1}. ${t.name} | ${t.route} | свободно: $free/${t.seats.size}"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  поездов нет" else info).map(_ => s)

def showTicketsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.soldTickets.map { t =>
    s"  #${t.id} ${t.route} ${t.classType} место=${t.seat} цена=${t.price} багаж=${t.baggageCost}"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  билетов нет" else info).map(_ => s)

def bookTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTrainsAction(s, cfg)
    trainNum  <- readInt("Номер поезда: ", -1)
    trainName  = if trainNum >= 1 && trainNum <= s.trains.size
                 then s.trains(trainNum - 1).name
                 else ""
    // показываем свободные места выбранного поезда
    _         <- IO.writeLine(
                      s.trains.find(_.name == trainName).map { t =>
                        val freeByRow = t.seats
                          .filter(!_._2)
                          .keys.toList
                          .groupBy(seat => seat.dropRight(1).toIntOption.getOrElse(0))
                          .toList.sortBy(_._1)
                          .map { case (row, seats) => s"  Ряд $row: ${seats.sorted.mkString("  ")}" }
                          .mkString("\n")
                        s"Свободные места:\n$freeByRow"
                      }.getOrElse("Поезд не найден")
                    )
    seat      <- readStr("Место: ")
    clsStr    <- readStr("Класс (economy/business): ")
    cls        = if clsStr.toLowerCase.startsWith("b") then ClassType.Business else ClassType.Economy
    baggage   <- readDouble("Вес багажа кг (0 если нет): ", 0.0)
    res       <- runState(s, FuncState.bookTicket(trainName, seat, cls, baggage)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

def cancelTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _        <- showTicketsAction(s, cfg)
    id       <- readInt("Номер билета: ", -1)
    res      <- runState(s, FuncState.cancelTicket(id)(cfg))
    (ns, log) = res
    _        <- printLog(log)
  yield ns

def addTrainAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    name   <- readStr("Название поезда: ")
    route  <- readStr("Маршрут (напр. Moscow-SPb): ")
    nSeats <- readInt("Количество рядов: ", 10)
    seats   = (1 to nSeats).flatMap { row =>
                Seq("A", "B", "C", "D").map(col => s"$row$col" -> false)
              }.toMap
    train   = Train(name, route, seats)
    res    <- runState(s, FuncState.addTrain(train))
    (ns, log) = res
    _      <- printLog(log)
  yield ns

def nextDayAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    res      <- runState(s, FuncState.nextDay)
    (ns, log) = res
    _        <- printLog(log)
  yield ns

// показывает как работает Writer отдельно от State
def writerDemoAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val w1 = FuncWriter.logRouteChoice("Moscow-SPb")
  val w2 = FuncWriter.logPriceCalc("Moscow-SPb", 3500, 200)
  val w3 = FuncWriter.logSeatAssigned("2A", "Express-1")
  val w4 = FuncWriter.logRefund(1, 3150)
  val logs = w1.log ++ w2.log ++ w3.log ++ w4.log
  for
    _ <- IO.writeLine("--- Writer demo ---")
    _ <- printLog(logs)
  yield s
  
def removeTrainAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _        <- showTrainsAction(s, cfg)
    trainNum <- readInt("Номер поезда для удаления: ", -1)
    trainName = if trainNum >= 1 && trainNum <= s.trains.size
                then s.trains(trainNum - 1).name
                else ""
    res      <- runState(s, FuncState.removeTrain(trainName))
    (ns, log) = res
    _        <- printLog(log)
  yield ns