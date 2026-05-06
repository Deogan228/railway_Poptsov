package monads

case class Writer[A](value: A, log: Vector[String])

object Writer:
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
  def tell(msg: String): Writer[Unit] = Writer((), Vector(msg))

given Monad[Writer] with
  override def pure[A](a: A): Writer[A] = Writer.pure(a)
  override def flatMap[A, B](ma: Writer[A])(f: A => Writer[B]): Writer[B] =
    val nw = f(ma.value)
    Writer(nw.value, ma.log ++ nw.log)