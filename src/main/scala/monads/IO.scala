package monads

// IO оборачивает эффект в ленивую функцию, ничего не запускается до unsafeRun
case class IO[A](unsafeRun: () => A)

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def delay[A](a: => A): IO[A] = IO(() => a)
  val readLine: IO[String] = IO(() => scala.io.StdIn.readLine())
  def writeLine(s: String): IO[Unit] = IO(() => Predef.println(s))
  def write(s: String): IO[Unit] = IO(() => Predef.print(s))

given Monad[IO] with
  override def pure[A](a: A): IO[A] = IO.pure(a)
  // запускаем первый эффект, результат передаём в f, запускаем второй
  override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
    IO(() => f(ma.unsafeRun()).unsafeRun())
