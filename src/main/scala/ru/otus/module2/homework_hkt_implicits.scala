package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_]]{
    def map[A,B](fa: F[A])(f: A => B): F[B]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Bindable{

    //имплисит значения для конкретных контейнеров.
    implicit val optToBindable: Bindable[Option] = new Bindable[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    implicit val listToBindable: Bindable[List] = new Bindable[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }


    //tupleF с implicit аргументами
    def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit ev: Bindable[F]): F[(A,B)] =
      ev.flatMap(fa)(a => ev.map(fb)(b => (a,b)))

  }

}