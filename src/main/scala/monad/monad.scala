package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] //= ???

    def flatMap[R](f: A => Wrap[R]): Wrap[R] =
      this match {
        case EmptyWrap => EmptyWrap
        case NonEmptyWrap(value) => f(value)
      }


    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = {
      flatMap(a => pure(f(a)))
    }

    def withFilter(f: A => Boolean): Wrap[A] = {
      this match {
        case EmptyWrap => EmptyWrap
        case NonEmptyWrap(value) =>
          if (f(value))
            NonEmptyWrap(value)
          else
            EmptyWrap
      }
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
    override def pure[A](x: A): Wrap[A] = NonEmptyWrap(x)
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
    override def pure[A](x: A): Wrap[A] = EmptyWrap
  } // bottom, null element

}