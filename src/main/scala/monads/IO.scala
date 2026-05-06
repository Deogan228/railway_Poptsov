package monads

// Монада ввода-вывода: оборачивает побочные эффекты в ленивое вычисление
case class IO[A](unsafeRun: () => A)

object IO:
  // Чистое значение без побочных эффектов
  def pure[A](a: A): IO[A] = IO(() => a)
  // Отложенное вычисление (ленивое)
  def delay[A](a: => A): IO[A] = IO(() => a)
  // Чтение строки из консоли
  val readLine: IO[String] = IO(() => scala.io.StdIn.readLine())
  // Вывод строки с переносом
  def writeLine(s: String): IO[Unit] = IO(() => Predef.println(s))
  // Вывод строки без переноса
  def write(s: String): IO[Unit] = IO(() => Predef.print(s))

// Экземпляр монады для IO
given Monad[IO] with
  override def pure[A](a: A): IO[A] = IO.pure(a)
  override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
    IO(() => f(ma.unsafeRun()).unsafeRun())