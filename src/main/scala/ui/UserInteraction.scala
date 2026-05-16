package ui

import domain.{TicketConfig, OfficeState}
import monads.{*, given}

// рекурсивный цикл: показать -> прочитать -> обработать -> повторить
trait UserInteraction:
  def show(state: OfficeState): String
  def readInput: IO[String] = IO.readLine
  def handleInput(input: String, state: OfficeState, cfg: TicketConfig): IO[Option[OfficeState]]

  def userInteractionLoop(state: OfficeState, cfg: TicketConfig): IO[OfficeState] =
    for
      _      <- IO.write(show(state))
      input  <- readInput
      next   <- handleInput(input, state, cfg)
      _      <- next match
        case Some(_) => IO.writeLine("\nНажмите Enter чтобы продолжить...").flatMap(_ => IO.readLine)
        case None    => IO.pure("")
      result <- next match
        case Some(ns) => userInteractionLoop(ns, cfg)
        case None     => IO.pure(state)
    yield result
