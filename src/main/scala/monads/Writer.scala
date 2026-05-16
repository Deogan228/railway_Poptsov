package monads

// Writer хранит результат и накопленный лог
// при flatMap логи склеиваются автоматически
case class Writer[A](value: A, log: Vector[String])

object Writer:
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
  def tell(msg: String): Writer[Unit] = Writer((), Vector(msg))

given Monad[Writer] with
  override def pure[A](a: A): Writer[A] = Writer.pure(a)
  override def flatMap[A, B](ma: Writer[A])(f: A => Writer[B]): Writer[B] =
    val next = f(ma.value)
    Writer(next.value, ma.log ++ next.log)
