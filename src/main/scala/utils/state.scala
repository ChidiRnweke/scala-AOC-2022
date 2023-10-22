package utils

opaque type State[S, +A] = S => (A, S)
object State:
  extension [S, A](underlying: State[S, A])
    def run(input: S): (A, S) = underlying(input)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      state =>
        val (a, s1) = run(state)
        val (b, s2) = f(a).run(s1)
        (b, s2)

    def map[B](f: A => B): State[S, B] =
      state =>
        val (a, s1) = run(state)
        (f(a), s1)

    def >>[B](next: => State[S, B]): State[S, B] =
      flatMap(_ => next)
  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: S): State[S, Unit] = _ => ((), s)
  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)
