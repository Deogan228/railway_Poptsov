package domain

import monads.{*, given}

// Доступ к конфигурации кассы через монаду Reader
object FuncReader:

  // Стоимость билета по маршруту и классу обслуживания
  def ticketPrice(route: String, classType: ClassType): Reader[TicketConfig, Option[Double]] =
    Reader { cfg =>
      cfg.tariffs.get(route).map { t =>
        classType match
          case ClassType.Economy  => t.economy
          case ClassType.Business => t.business
      }
    }

  // Доплата за багаж по весу (кг)
  def baggageCost(weight: Double): Reader[TicketConfig, Double] =
    Reader { cfg =>
      if weight <= 0 then 0.0 else weight * cfg.baggagePerKg
    }

  // Проверка: свободно ли указанное место в поезде
  def seatAvailable(train: Train, seat: String): Reader[TicketConfig, Boolean] =
    Reader { _ =>
      train.seats.get(seat) match
        case Some(occupied) => !occupied
        case None           => false
    }

  // Сумма возврата за билет (цена + багаж минус штраф)
  def refundAmount(ticket: Ticket): Reader[TicketConfig, Double] =
    Reader { cfg =>
      val total = ticket.price + ticket.baggageCost
      total * (1.0 - cfg.refundPenaltyPercent)
    }
