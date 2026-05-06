package domain

// Ticket class
enum ClassType:
  case Economy, Business

// Route tariff
case class RouteTariff(
  route: String,          // e.g. "Moscow-SPb"
  economy: Double,        // economy price
  business: Double        // business price
)

// Ticket office configuration (Reader environment)
case class TicketConfig(
  tariffs: Map[String, RouteTariff],   // route name -> tariff
  baggagePerKg: Double,               // surcharge per kg of baggage
  seatRule: String,                    // seat assignment rule: "window" | "aisle" | "any"
  refundPenaltyPercent: Double         // refund penalty as fraction 0..1
)

// A sold ticket
case class Ticket(
  id: Int,
  route: String,
  classType: ClassType,
  seat: String,           // e.g. "3A"
  price: Double,
  baggageWeight: Double,
  baggageCost: Double
)

// A train with seat availability
case class Train(
  name: String,                     // e.g. "Train 42"
  route: String,
  seats: Map[String, Boolean]       // seat label -> occupied?
)

// Global mutable state of the ticket office
case class OfficeState(
  trains: List[Train],
  soldTickets: List[Ticket],
  revenue: Double,
  nextTicketId: Int
)
