package domain

import monads.{*, given}

// Переходы состояния кассы с логированием через Writer
object FuncState:

  // Бронирование билета: выбор поезда, места, класса и багажа
  def bookTicket(trainName: String, seat: String, classType: ClassType,
                 baggageWeight: Double)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.trains.find(_.name == trainName) match
        case None =>
          (s, Writer.tell(s"Ошибка: поезд $trainName не найден").log)
        case Some(train) =>
          val available = FuncReader.seatAvailable(train, seat).run(cfg)
          if !available then
            (s, Writer.tell(s"Ошибка: место $seat в поезде $trainName занято или не существует").log)
          else
            FuncReader.ticketPrice(train.route, classType).run(cfg) match
              case None =>
                (s, Writer.tell(s"Ошибка: тариф для маршрута ${train.route} не найден").log)
              case Some(price) =>
                val bagCost = FuncReader.baggageCost(baggageWeight).run(cfg)
                val ticket = Ticket(
                  id = s.nextTicketId,
                  route = train.route,
                  classType = classType,
                  seat = seat,
                  price = price,
                  baggageWeight = baggageWeight,
                  baggageCost = bagCost
                )
                val updatedTrain = train.copy(seats = train.seats.updated(seat, true))
                val updatedTrains = s.trains.map(t => if t.name == trainName then updatedTrain else t)
                val ns = s.copy(
                  trains = updatedTrains,
                  soldTickets = s.soldTickets :+ ticket,
                  revenue = s.revenue + price + bagCost,
                  nextTicketId = s.nextTicketId + 1
                )
                val log = Vector(
                  s"Билет #${ticket.id} оформлен: $trainName, место $seat, $classType, цена=$price, багаж=$bagCost"
                )
                (ns, log)
    }

  // Отмена билета по номеру с расчётом возврата
  def cancelTicket(ticketId: Int)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.soldTickets.find(_.id == ticketId) match
        case None =>
          (s, Writer.tell(s"Ошибка: билет #$ticketId не найден").log)
        case Some(ticket) =>
          val refund = FuncReader.refundAmount(ticket).run(cfg)
          // освобождаем место в поезде
          val updatedTrains = s.trains.map { train =>
            if train.route == ticket.route then
              train.copy(seats = train.seats.updated(ticket.seat, false))
            else train
          }
          val ns = s.copy(
            trains = updatedTrains,
            soldTickets = s.soldTickets.filterNot(_.id == ticketId),
            revenue = s.revenue - refund
          )
          val log = Vector(
            s"Билет #$ticketId отменён, возврат=$refund (штраф=${cfg.refundPenaltyPercent * 100}%)"
          )
          (ns, log)
    }

  // Добавление нового поезда в систему
  def addTrain(train: Train): State[OfficeState, Vector[String]] =
    State { s =>
      if s.trains.exists(_.name == train.name) then
        (s, Writer.tell(s"Ошибка: поезд ${train.name} уже существует").log)
      else
        val ns = s.copy(trains = s.trains :+ train)
        (ns, Writer.tell(s"Поезд ${train.name} добавлен, маршрут=${train.route}, мест=${train.seats.size}").log)
    }

  // Переход к следующему дню: сброс проданных билетов и выручки
  def nextDay: State[OfficeState, Vector[String]] =
    State { s =>
      val ns = s.copy(soldTickets = List.empty, revenue = 0.0)
      (ns, Writer.tell(s"Новый день. Выручка за прошлый день: ${s.revenue}, билеты очищены.").log)
    }