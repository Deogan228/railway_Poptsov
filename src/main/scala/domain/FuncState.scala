package domain

import monads.{*, given}

// State transitions for the ticket office, with Writer logging
object FuncState:

  // Book a ticket: choose train, seat, route, class, baggage
  def bookTicket(trainName: String, seat: String, classType: ClassType,
                 baggageWeight: Double)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.trains.find(_.name == trainName) match
        case None =>
          (s, Writer.tell(s"Error: train $trainName not found").log)
        case Some(train) =>
          val available = FuncReader.seatAvailable(train, seat).run(cfg)
          if !available then
            (s, Writer.tell(s"Error: seat $seat on $trainName is not available").log)
          else
            FuncReader.ticketPrice(train.route, classType).run(cfg) match
              case None =>
                (s, Writer.tell(s"Error: no tariff for route ${train.route}").log)
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
                  s"Ticket #${ticket.id} booked: $trainName, seat $seat, ${classType}, price=$price, baggage=$bagCost"
                )
                (ns, log)
    }

  // Cancel a ticket by id
  def cancelTicket(ticketId: Int)(cfg: TicketConfig): State[OfficeState, Vector[String]] =
    State { s =>
      s.soldTickets.find(_.id == ticketId) match
        case None =>
          (s, Writer.tell(s"Error: ticket #$ticketId not found").log)
        case Some(ticket) =>
          val refund = FuncReader.refundAmount(ticket).run(cfg)
          // free the seat
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
            s"Ticket #$ticketId cancelled, refund=$refund (penalty=${cfg.refundPenaltyPercent * 100}%)"
          )
          (ns, log)
    }

  // Add a new train
  def addTrain(train: Train): State[OfficeState, Vector[String]] =
    State { s =>
      if s.trains.exists(_.name == train.name) then
        (s, Writer.tell(s"Error: train ${train.name} already exists").log)
      else
        val ns = s.copy(trains = s.trains :+ train)
        (ns, Writer.tell(s"Train ${train.name} added, route=${train.route}, seats=${train.seats.size}").log)
    }

  // Advance to next day: clear sold tickets, reset revenue counter
  def nextDay: State[OfficeState, Vector[String]] =
    State { s =>
      val ns = s.copy(soldTickets = List.empty, revenue = 0.0)
      (ns, Writer.tell(s"New day started. Previous revenue: ${s.revenue}, tickets cleared.").log)
    }
