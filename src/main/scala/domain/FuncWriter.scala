package domain

import monads.{*, given}

// логирование через Writer, используется в writerDemoAction
object FuncWriter:

  def logRouteChoice(route: String): Writer[String] =
    for
      _ <- Writer.tell(s"Выбран маршрут: $route")
    yield s"Маршрут: $route"

  def logPriceCalc(route: String, price: Double, baggage: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Расчёт: билет=$price багаж=$baggage итого=${price + baggage}")
    yield s"Итого: ${price + baggage}"

  def logSeatAssigned(seat: String, train: String): Writer[String] =
    for
      _ <- Writer.tell(s"Место $seat в поезде $train назначено")
    yield s"$seat / $train"

  def logRefund(ticketId: Int, amount: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Возврат #$ticketId на сумму $amount")
    yield s"Возврат: $amount"
