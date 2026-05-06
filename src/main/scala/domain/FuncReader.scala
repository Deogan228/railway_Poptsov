package domain

import monads.{*, given}

// Configuration access through Reader
object FuncReader:

  // Ticket price for a given route and class
  def ticketPrice(route: String, classType: ClassType): Reader[TicketConfig, Option[Double]] =
    Reader { cfg =>
      cfg.tariffs.get(route).map { t =>
        classType match
          case ClassType.Economy  => t.economy
          case ClassType.Business => t.business
      }
    }

  // Baggage surcharge for a given weight
  def baggageCost(weight: Double): Reader[TicketConfig, Double] =
    Reader { cfg =>
      if weight <= 0 then 0.0 else weight * cfg.baggagePerKg
    }

  // Check if a seat is available on a train
  def seatAvailable(train: Train, seat: String): Reader[TicketConfig, Boolean] =
    Reader { _ =>
      train.seats.get(seat) match
        case Some(occupied) => !occupied
        case None           => false
    }

  // Refund amount for a ticket (price + baggage minus penalty)
  def refundAmount(ticket: Ticket): Reader[TicketConfig, Double] =
    Reader { cfg =>
      val total = ticket.price + ticket.baggageCost
      total * (1.0 - cfg.refundPenaltyPercent)
    }
