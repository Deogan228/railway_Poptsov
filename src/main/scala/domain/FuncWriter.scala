package domain

import monads.{*, given}

// Logging key events through Writer
object FuncWriter:

  def logRouteChoice(route: String): Writer[String] =
    for
      _ <- Writer.tell(s"Route selected: $route")
    yield s"Route: $route"

  def logPriceCalc(route: String, price: Double, baggage: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Price calculated: ticket=$price, baggage=$baggage, total=${price + baggage}")
    yield s"Total: ${price + baggage}"

  def logSeatAssigned(seat: String, train: String): Writer[String] =
    for
      _ <- Writer.tell(s"Seat $seat assigned on $train")
    yield s"Seat $seat on $train"

  def logRefund(ticketId: Int, amount: Double): Writer[String] =
    for
      _ <- Writer.tell(s"Refund ticket #$ticketId, amount=$amount")
    yield s"Refunded $amount for ticket #$ticketId"
