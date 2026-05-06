package ui

import domain.{TicketConfig, OfficeState}
import monads.{*, given}

// Dialog loop: show menu -> read input -> handle -> repeat or exit
trait UserInteraction:
  def show(state: OfficeState): String
  def readInput: IO[String] = IO.readLine
  def handleInput(input: String, state: OfficeState, cfg: TicketConfig): IO[Option[OfficeState]]

  def userInteractionLoop(state: OfficeState, cfg: TicketConfig): IO[OfficeState] =
    for
      _      <- IO.write(show(state))
      input  <- readInput
      next   <- handleInput(input, state, cfg)
      result <- next match
        case Some(ns) => userInteractionLoop(ns, cfg)
        case None     => IO.pure(state)
    yield result
