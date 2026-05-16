package monads

// State[S, A] = функция из старого состояния в (новое состояние, результат)
// вместо глобальных переменных
case class State[S, A](run: S => (S, A))

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](newS: S): State[S, Unit] = State(_ => (newS, ()))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
  def gets[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))

given [S]: Monad[[A] =>> State[S, A]] with
  override def pure[A](a: A): State[S, A] = State.pure(a)
  override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a) = ma.run(s)  // прогоняем первое, получаем промежуточное состояние
      f(a).run(s1)
    }
