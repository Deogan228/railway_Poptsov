package domain

import monads.{*, given}

// Логирование ключевых событий через монаду Writer
object FuncWriter:

  // Запись в лог выбора маршрута
  def logRouteChoice(route: String): Writer[String] =
    for
      _ <- Writer.tell(s"Выбран маршрут: $route")
    yield s"Маршрут: $route"

  // Запись в лог расчёта стоимости
  def logPriceCalc(route: String, price: Double, baggage: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Расчёт цены: билет=$price, багаж=$baggage, итого=${price + baggage}")
    yield s"Итого: ${price + baggage}"

  // Запись в лог назначения места
  def logSeatAssigned(seat: String, train: String): Writer[String] =
    for
      _ <- Writer.tell(s"Место $seat назначено в поезде $train")
    yield s"Место $seat в поезде $train"

  // Запись в лог возврата билета
  def logRefund(ticketId: Int, amount: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Возврат билета #$ticketId, сумма=$amount")
    yield s"Возвращено $amount за билет #$ticketId"
