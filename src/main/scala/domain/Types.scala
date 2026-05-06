package domain

// Класс обслуживания
enum ClassType:
  case Economy, Business

// Тариф по маршруту
case class RouteTariff(
  route: String,          // название маршрута, напр. "Moscow-SPb"
  economy: Double,        // цена эконом-класса
  business: Double        // цена бизнес-класса
)

// Конфигурация кассы (окружение для Reader)
case class TicketConfig(
  tariffs: Map[String, RouteTariff],   // маршрут -> тариф
  baggagePerKg: Double,               // доплата за кг багажа
  seatRule: String,                    // правило выбора места: "window" | "aisle" | "any"
  refundPenaltyPercent: Double         // штраф за возврат (доля от 0 до 1)
)

// Проданный билет
case class Ticket(
  id: Int,
  route: String,
  classType: ClassType,
  seat: String,           // напр. "3A"
  price: Double,
  baggageWeight: Double,
  baggageCost: Double
)

// Поезд с картой занятости мест
case class Train(
  name: String,                     // напр. "Express-1"
  route: String,
  seats: Map[String, Boolean]       // место -> занято?
)

// Глобальное состояние кассы
case class OfficeState(
  trains: List[Train],
  soldTickets: List[Ticket],
  revenue: Double,
  nextTicketId: Int
)