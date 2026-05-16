package domain

enum ClassType:
  case Economy, Business

case class RouteTariff(
  route: String,
  economy: Double,
  business: Double
)

// конфиг кассы, передаётся через Reader
case class TicketConfig(
  tariffs: Map[String, RouteTariff],
  baggagePerKg: Double,
  seatRule: String,                    // "window" | "aisle" | "any", пока не используется
  refundPenaltyPercent: Double         // доля штрафа, напр. 0.15 = 15%
)

case class Ticket(
  id: Int,
  route: String,
  classType: ClassType,
  seat: String,
  price: Double,
  baggageWeight: Double,
  baggageCost: Double
)

// seats: место -> занято (true) или свободно (false)
case class Train(
  name: String,
  route: String,
  seats: Map[String, Boolean]
)

// всё состояние кассы в одном месте
case class OfficeState(
  trains: List[Train],
  soldTickets: List[Ticket],
  revenue: Double,
  nextTicketId: Int
)
