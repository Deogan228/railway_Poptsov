package ui

import domain.{TicketConfig, OfficeState}
import monads.IO

// Menu item: title and action over state
trait MenuOption:
  def title: String
  def execute(state: OfficeState, cfg: TicketConfig): IO[OfficeState]
