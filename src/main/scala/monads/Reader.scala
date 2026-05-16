package monads

// Reader[Env, A] это просто обёртка над функцией Env => A
// удобно когда надо читать из конфига не пробрасывая его везде руками
case class Reader[Env, A](run: Env => A)

object Reader:
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)
  def ask[Env]: Reader[Env, Env] = Reader(env => env)
  // вытащить часть окружения
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(f)

given [Env]: Monad[[A] =>> Reader[Env, A]] with
  override def pure[A](a: A): Reader[Env, A] = Reader.pure(a)
  override def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(ma.run(env)).run(env))
