package monads

case class Reader[Env, A](run: Env => A)

object Reader:
  def pure[Env, A](a: A): Reader[Env, A] = Reader(env => a)
  def ask[Env]: Reader[Env, Env] = Reader(env => env)
  def asks[Env, A](f: Env => A): Reader[Env, A] = Reader(env => f(env))

given [Env]: Monad[[A] =>> Reader[Env, A]] with
  override def pure[A](a: A): Reader[Env, A] = Reader.pure(a)
  override def flatMap[A, B](ma: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(ma.run(env)).run(env))