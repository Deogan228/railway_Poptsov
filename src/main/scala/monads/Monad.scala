package monads

// базовый трейт, pure кладёт значение в контекст
trait Applicative[F[_]]:
  def pure[X](x: X): F[X]

// монада = аппликатив + flatMap для цепочки вычислений
trait Monad[M[_]] extends Applicative[M]:
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  // map можно выразить через flatMap, не нужно переопределять
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

// чтобы писать mx.flatMap(...) и for-comprehension
extension [M[_], X](mx: M[X])(using monad: Monad[M])
  def flatMap[Y](f: X => M[Y]): M[Y] = monad.flatMap(mx)(f)
  def map[Y](f: X => Y): M[Y] = monad.map(mx)(f)
