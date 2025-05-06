package ru.otus.module2

import scala.util.{Failure, Success, Try}

package object catsHomework {
  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }

  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

  lazy val tryME = new MonadError[Try, Throwable]{
    // Поднимаем ошибку в контекст `F`:
    override def raiseError[A](e: Throwable): Try[A] = Failure[A](e)

    // Обработка ошибки, потенциальное восстановление:
    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] =
      fa match {
        case Success(_) => fa
        case Failure(ex) => f(ex)
      }

    // Обработка ошибок, восстановление от них:
    override def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa.recover(f(_))
     /*fa match {
        case Success(_) => fa
        case Failure(ex) => pure(f(ex))
      }*/

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    override def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] =
      fa match {
        case Success(value) if f(value) => fa
        case Success(value) if !f(value) => Failure(e)
        case Failure(_) => Failure(e)
      }

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def pure[A](v: A): Try[A] = Try(v)
  }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */
  type EitherString[A] = Either[String, A]

  lazy val eitherME = new MonadError[EitherString, Throwable]{

    override def raiseError[A](e: Throwable): EitherString[A] = Left(e.getMessage)

    override def handleErrorWith[A](fa: EitherString[A])(f: Throwable => EitherString[A]): EitherString[A] =
      fa match {
        case Left(l) => f(new Throwable(l))
        case Right(_) => fa
      }

    override def handleError[A](fa: EitherString[A])(f: Throwable => A): EitherString[A] =
      fa match {
        case Left(l) => pure(f(new Throwable(l)))
        case Right(_) => fa
      }

    override def ensure[A](fa: EitherString[A])(e: Throwable)(f: A => Boolean): EitherString[A] =
      fa match {
        case Left(_) => Left(e.getMessage)
        case Right(value) if f(value) => fa
        case Right(value) if !f(value) => Left(e.getMessage)
      }

    override def flatMap[A, B](fa: EitherString[A])(f: A => EitherString[B]): EitherString[B] =
      fa.flatMap(f)

    override def pure[A](v: A): EitherString[A] = Right(v)
  }

}

import catsHomework._

object MainApp extends App {
  println("Hello world")

  //Try case
  val sTry: Try[Int] = Try(5)
  val fTry: Try[Int] = Failure(new Throwable("Error from fTry"))

  println(tryME.raiseError(new Throwable("check raiseError")))
  println("------------------------------")
  println(tryME.handleErrorWith(sTry)(_ => Try(6))) //т.к. нет ошибки, нет и обработки                   Success(5)
  println(tryME.handleErrorWith(fTry)(_ => Try(7))) //есть ошибка, игнорируем её и производим новый Try  Success(7)

  private val recoverFunction: Throwable => Int = e => {
    println(s"    LOG: ${e.getMessage}")
    8
  }
  println("------------------------------")
  println(tryME.handleError(sTry)(recoverFunction))//т.к. нет ошибки, нет и обработки                   Success(5)
  println(tryME.handleError(fTry)(recoverFunction))//есть ошибка, логируем её и возвращаем значение     Success(8)

  println("------------------------------")
  val errorStringT = "Succeed value in Try <= 5 or Failure case"
  println(tryME.ensure(sTry)(new Throwable(errorStringT))(_ > 5))    // Failure(java.lang.Throwable: Succeed value in Try <= 5 or Failure case)
  println(tryME.ensure(sTry)(new Throwable(errorStringT))(_ <= 5))   // Success(5)
  println(tryME.ensure(fTry)(new Throwable(errorStringT))(_ > 5))    // Failure(java.lang.Throwable: Succeed value in Try <= 5 or Failure case)
  println(tryME.ensure(fTry)(new Throwable(errorStringT))(_ <= 5))   // Failure(java.lang.Throwable: Succeed value in Try <= 5 or Failure case)

  println("------------------------------")

  //Either case
  val lEither: Either[String,Int] = Left("error in lEither")
  val rEither: Either[String,Int] = Right(5)
  println(eitherME.raiseError(new Throwable("check raiseError")))
  println("------------------------------")
  println(eitherME.handleErrorWith(lEither)(_ => Right(6))) //есть ошибка, игнорируем её и производим новый  Right(6)
  println(eitherME.handleErrorWith(rEither)(_ => Right(7))) //нет ошибки, возвращается  rEither              Right(5)
  println("------------------------------")
  println(eitherME.handleError(lEither)(recoverFunction))//есть ошибка, вызывается обработчик recoverFunction Right(8)
  println(eitherME.handleError(rEither)(recoverFunction))//нет ошибки, возвращается  rEither                  Right(5)

  println("------------------------------")
  val errorStringE = "Succeed value in Either.right <= 5 or Left case"
  println(eitherME.ensure(lEither)(new Throwable(errorStringE))(_ > 5))    // Left(Succeed value in Either.right <= 5 or Left case)
  println(eitherME.ensure(lEither)(new Throwable(errorStringE))(_ <= 5))   // Left(Succeed value in Either.right <= 5 or Left case)
  println(eitherME.ensure(rEither)(new Throwable(errorStringE))(_ > 5))    // Left(Succeed value in Either.right <= 5 or Left case)
  println(eitherME.ensure(rEither)(new Throwable(errorStringE))(_ <= 5))   // проверка успешна  Right(5)

}
