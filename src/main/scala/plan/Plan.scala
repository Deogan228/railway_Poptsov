package plan

import monads.{*, given}
import domain.{OfficeState, TicketConfig, ClassType, Train, FuncState, FuncWriter}

// Преобразование State-действия в IO
def runState[A](s: OfficeState, action: State[OfficeState, A]): IO[(OfficeState, A)] =
  IO.delay(action.run(s))

// Вывод накопленного лога в консоль
def printLog(log: Vector[String]): IO[Unit] =
  log.foldLeft(IO.pure(()))((acc, line) => acc.flatMap(_ => IO.writeLine("  " + line)))

// Чтение целого числа с подсказкой и значением по умолчанию
def readInt(prompt: String, fallback: Int): IO[Int] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toIntOption.getOrElse(fallback)

// Чтение дробного числа с подсказкой и значением по умолчанию
def readDouble(prompt: String, fallback: Double): IO[Double] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim.toDoubleOption.getOrElse(fallback)

// Чтение строки с подсказкой
def readStr(prompt: String): IO[String] =
  for
    _   <- IO.write(prompt)
    str <- IO.readLine
  yield str.trim

// Показать список доступных поездов
def showTrainsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.trains.zipWithIndex.map { case (t, i) =>
    val free = t.seats.count(!_._2)
    val total = t.seats.size
    s"  ${i + 1}. ${t.name} | маршрут: ${t.route} | места: $free/$total свободно"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  Нет доступных поездов" else info).map(_ => s)

// Показать проданные билеты
def showTicketsAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val info = s.soldTickets.map { t =>
    s"  #${t.id} ${t.route} ${t.classType} место=${t.seat} цена=${t.price} багаж=${t.baggageCost}"
  }.mkString("\n")
  IO.writeLine(if info.isEmpty then "  Билеты не проданы" else info).map(_ => s)

// Покупка билета: интерактивный ввод данных
def bookTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTrainsAction(s, cfg)
    trainName <- readStr("Название поезда: ")
    seat      <- readStr("Место (напр. 1A): ")
    clsStr    <- readStr("Класс (economy/business): ")
    cls        = if clsStr.toLowerCase.startsWith("b") then ClassType.Business else ClassType.Economy
    baggage   <- readDouble("Вес багажа кг (0 — без багажа): ", 0.0)
    res       <- runState(s, FuncState.bookTicket(trainName, seat, cls, baggage)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Возврат билета: интерактивный ввод номера
def cancelTicketAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    _         <- showTicketsAction(s, cfg)
    ticketId  <- readInt("Номер билета для возврата: ", -1)
    res       <- runState(s, FuncState.cancelTicket(ticketId)(cfg))
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Добавление нового поезда в систему
def addTrainAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    name    <- readStr("Название поезда: ")
    route   <- readStr("Маршрут (напр. Moscow-SPb): ")
    nSeats  <- readInt("Количество рядов: ", 10)
    seats    = (1 to nSeats).flatMap { row =>
                 Seq("A", "B", "C", "D").map(col => s"$row$col" -> false)
               }.toMap
    train    = Train(name, route, seats)
    res     <- runState(s, FuncState.addTrain(train))
    (ns, log) = res
    _       <- printLog(log)
  yield ns

// Переход к следующему дню
def nextDayAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  for
    res       <- runState(s, FuncState.nextDay)
    (ns, log)  = res
    _         <- printLog(log)
  yield ns

// Демонстрация чистого Writer (без изменения состояния)
def writerDemoAction(s: OfficeState, cfg: TicketConfig): IO[OfficeState] =
  val w1 = FuncWriter.logRouteChoice("Moscow-SPb")
  val w2 = FuncWriter.logPriceCalc("Moscow-SPb", 3500, 200)
  val w3 = FuncWriter.logSeatAssigned("2A", "Express-1")
  val w4 = FuncWriter.logRefund(1, 3150)
  val allLogs = w1.log ++ w2.log ++ w3.log ++ w4.log
  IO.writeLine("[Демонстрация Writer]").flatMap(_ =>
    allLogs.foldLeft(IO.pure(()))((io, line) => io.flatMap(_ => IO.writeLine("  " + line)))
  ).map(_ => s)