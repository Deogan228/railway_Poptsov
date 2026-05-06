package ui

import domain.{TicketConfig, OfficeState}
import monads.IO

// Пункт меню: заголовок и действие над состоянием кассы
trait MenuOption:
  def title: String
  def execute(state: OfficeState, cfg: TicketConfig): IO[OfficeState]