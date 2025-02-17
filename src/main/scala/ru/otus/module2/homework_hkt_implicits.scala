package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_], A]{
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  object Bindable{
    implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)
      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(a => f(a))
    }
    implicit def listBindable[A](lst: List[A]): Bindable[List, A] = new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = lst.map(f)
      override def flatMap[B](f: A => List[B]): List[B] = lst.flatMap(a => f(a))
    }
    def tuplef[F[_], A, B](fa: Bindable[F,A], fb: Bindable[F,B]): F[(A,B)] =
      fa.flatMap(a => fb.map(b => (a,b)))
  }

}

import homework_hkt_implicits.Bindable._

tuplef(Some(123),Some("hello"))
tuplef(List(123),List("hello"))