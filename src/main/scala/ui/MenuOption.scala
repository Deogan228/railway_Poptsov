package ui

import domain.{TicketConfig, OfficeState}
import monads.IO

trait MenuOption:
  def title: String
  def execute(state: OfficeState, cfg: TicketConfig): IO[OfficeState]
