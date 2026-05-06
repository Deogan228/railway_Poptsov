package ui

import domain.{TicketConfig, OfficeState}
import monads.{*, given}

// Лист дерева: пункт меню с прямым действием
case class MenuLeaf(
  title: String,
  action: (OfficeState, TicketConfig) => IO[OfficeState]
) extends MenuOption:
  def execute(state: OfficeState, cfg: TicketConfig): IO[OfficeState] =
    action(state, cfg)

// Узел дерева: подменю с собственным циклом ввода
case class MenuTreeNode(title: String, options: Seq[MenuOption])
  extends MenuOption with UserInteraction:

  def execute(state: OfficeState, cfg: TicketConfig): IO[OfficeState] =
    userInteractionLoop(state, cfg)

  // Отображение состояния кассы и списка пунктов меню
  def show(state: OfficeState): String =
    val items = options.zipWithIndex
      .map { case (opt, i) => s"${i + 1}  ${opt.title}" }
      .mkString("\n")
    val trainInfo = state.trains.map(t =>
      val free = t.seats.count(!_._2)
      s"${t.name}(${t.route}, свободно=${free})"
    ).mkString(", ")
    s"\n--- $title (поезда: $trainInfo, выручка: ${state.revenue}, билетов: ${state.soldTickets.size}) ---\n$items\n0  выход\nвыбор: "

  // Обработка ввода: 0 — выход, номер — выполнение пункта
  def handleInput(input: String, state: OfficeState, cfg: TicketConfig): IO[Option[OfficeState]] =
    input.trim.toIntOption match
      case Some(0) => IO.pure(None)
      case Some(i) if i >= 1 && i <= options.size =>
        options(i - 1).execute(state, cfg).map(ns => Some(ns))
      case _ =>
        IO.writeLine("  неизвестная команда").map(_ => Some(state))