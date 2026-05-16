package domain

import monads.{*, given}

object FuncState:

  def bookTicket(trainName: String, seat: String, classType: ClassType,
                 baggageWeight: Double)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.trains.find(_.name == trainName) match
        case None =>
          (s, Writer.tell(s"Ошибка: поезд $trainName не найден").log)

        case Some(train) =>
          if !FuncReader.seatAvailable(train, seat).run(cfg) then
            (s, Writer.tell(s"Ошибка: место $seat занято или не существует").log)
          else
            FuncReader.ticketPrice(train.route, classType).run(cfg) match
              case None =>
                (s, Writer.tell(s"Ошибка: нет тарифа для ${train.route}").log)
              case Some(price) =>
                val bagCost = FuncReader.baggageCost(baggageWeight).run(cfg)
                val ticket = Ticket(
                  id            = s.nextTicketId,
                  route         = train.route,
                  classType     = classType,
                  seat          = seat,
                  price         = price,
                  baggageWeight = baggageWeight,
                  baggageCost   = bagCost
                )
                val updatedTrain  = train.copy(seats = train.seats.updated(seat, true))
                val updatedTrains = s.trains.map(t => if t.name == trainName then updatedTrain else t)
                val ns = s.copy(
                  trains       = updatedTrains,
                  soldTickets  = s.soldTickets :+ ticket,
                  revenue      = s.revenue + price + bagCost,
                  nextTicketId = s.nextTicketId + 1
                )
                (ns, Vector(s"Билет #${ticket.id} оформлен: $trainName место=$seat класс=$classType цена=$price багаж=$bagCost"))
    }

  // отмена по id, возвращает деньги за вычетом штрафа
  def cancelTicket(ticketId: Int)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.soldTickets.find(_.id == ticketId) match
        case None =>
          (s, Writer.tell(s"Ошибка: билет #$ticketId не найден").log)
        case Some(ticket) =>
          val refund = FuncReader.refundAmount(ticket).run(cfg)
          val updatedTrains = s.trains.map { t =>
            if t.route == ticket.route then t.copy(seats = t.seats.updated(ticket.seat, false))
            else t
          }
          val ns = s.copy(
            trains      = updatedTrains,
            soldTickets = s.soldTickets.filterNot(_.id == ticketId),
            revenue     = s.revenue - refund
          )
          (ns, Vector(s"Билет #$ticketId отменён, возврат=$refund (штраф=${cfg.refundPenaltyPercent * 100}%)"))
    }

  def addTrain(train: Train): State[OfficeState, Vector[String]] =
    State { s =>
      if s.trains.exists(_.name == train.name) then
        (s, Writer.tell(s"Ошибка: поезд ${train.name} уже есть").log)
      else
        val ns = s.copy(trains = s.trains :+ train)
        (ns, Writer.tell(s"Поезд ${train.name} добавлен, маршрут=${train.route} мест=${train.seats.size}").log)
    }

  def nextDay: State[OfficeState, Vector[String]] =
    State { s =>
      val ns = s.copy(soldTickets = List.empty, revenue = 0.0)
      (ns, Writer.tell(s"Новый день. Выручка за вчера: ${s.revenue}, билеты сброшены.").log)
    }

  def removeTrain(trainName: String): State[OfficeState, Vector[String]] =
    State { s =>
      if !s.trains.exists(_.name == trainName) then
        (s, Writer.tell(s"Ошибка: поезд $trainName не найден").log)
      else
        val trainRoute     = s.trains.find(_.name == trainName).map(_.route).getOrElse("")
        val hasSoldTickets = s.soldTickets.exists(_.route == trainRoute)
        if hasSoldTickets then
          (s, Writer.tell(s"Ошибка: на поезд $trainName есть проданные билеты").log)
        else
          val ns = s.copy(trains = s.trains.filterNot(_.name == trainName))
          (ns, Writer.tell(s"Поезд $trainName удалён").log)
    }
