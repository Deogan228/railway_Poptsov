package monads

// Монада Writer: вычисления с накоплением лога
case class Writer[A](value: A, log: Vector[String])

object Writer:
  // Чистое значение без записей в логе
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
  // Добавить сообщение в лог
  def tell(msg: String): Writer[Unit] = Writer((), Vector(msg))

// Экземпляр монады для Writer: при цепочке вычислений логи объединяются
given Monad[Writer] with
  override def pure[A](a: A): Writer[A] = Writer.pure(a)
  override def flatMap[A, B](ma: Writer[A])(f: A => Writer[B]): Writer[B] =
    val nw = f(ma.value)
    Writer(nw.value, ma.log ++ nw.log)