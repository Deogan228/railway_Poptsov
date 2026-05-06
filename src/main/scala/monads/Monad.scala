package monads

// Аппликативный функтор: умеет оборачивать значение в контекст
trait Applicative[F[_]]:
  def pure[X](x: X): F[X]

// Монада: аппликатив + цепочка вычислений через flatMap
trait Monad[M[_]] extends Applicative[M]:
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))

// Расширения для удобного синтаксиса: позволяют писать mx.flatMap(...) и mx.map(...)
extension [M[_], X](mx: M[X])(using monad: Monad[M])
  def flatMap[Y](f: X => M[Y]): M[Y] = monad.flatMap(mx)(f)
  def map[Y](f: X => Y): M[Y] = monad.map(mx)(f)