package monads

// Монада Reader: доступ к неизменяемому окружению (конфигурации)
case class Reader[Env, A](run: Env => A)

object Reader:
  // Чистое значение, не зависящее от окружения
  def pure[Env, A](a: A): Reader[Env, A] = Reader(env => a)
  // Получить всё окружение целиком
  def ask[Env]: Reader[Env, Env] = Reader(env => env)
  // Извлечь часть окружения через функцию
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(env => f(env))

// Экземпляр монады для Reader с фиксированным типом окружения
given [Env]: Monad[[A] =>> Reader[Env, A]] with
  override def pure[A](a: A): Reader[Env, A] = Reader.pure(a)
  override def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(ma.run(env)).run(env))