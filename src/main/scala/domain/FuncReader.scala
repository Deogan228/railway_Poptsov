package domain

import monads.{*, given}

// функции для чтения из конфига через Reader
object FuncReader:

  def ticketPrice(route: String, classType: ClassType): Reader[TicketConfig, Option[Double]] =
    Reader { cfg =>
      cfg.tariffs.get(route).map { t =>
        classType match
          case ClassType.Economy  => t.economy
          case ClassType.Business => t.business
      }
    }

  // если вес <= 0 считаем что багажа нет
  def baggageCost(weight: Double): Reader[TicketConfig, Double] =
    Reader { cfg =>
      if weight <= 0 then 0.0
      else weight * cfg.baggagePerKg
    }

  def seatAvailable(train: Train, seat: String): Reader[TicketConfig, Boolean] =
    Reader { _ =>
      train.seats.get(seat) match
        case Some(occupied) => !occupied
        case None           => false  // места вообще нет
    }

  // возврат = (цена + багаж) * (1 - штраф)
  def refundAmount(ticket: Ticket): Reader[TicketConfig, Double] =
    Reader { cfg =>
      val total = ticket.price + ticket.baggageCost
      total * (1.0 - cfg.refundPenaltyPercent)
    }
