package ru.otus.module3

import zio.{IO, Task, UIO, URIO}

import java.io.IOException


object zioErrorHandling {

  sealed trait Cause[+E]

  object Cause {

    final case class Fail[E](e: E) extends Cause[E]

    final case class Die(t: Throwable) extends Cause[Nothing]

  }


  case class ZIO[-R, +E, +A](run: R => Either[E, A]) {self =>

    /**
      * 
      * Базовый оператор для работы с ошибками
      */

    def foldM[R1 <: R, E1, B](
             failure: E => ZIO[R1, E1, B],
             success: A => ZIO[R1, E1, B]
             ): ZIO[R1, E1, B] =
      ZIO(r => this.run(r).fold(
        e => failure(e),
        a => success(a)
      ).run(r))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      foldM(
        e => ZIO(_ => Left(e)),
        v => f(v)
      )

    /**
     * Реализовать метод, который будет игнорировать ошибку в случае падения,
     * а в качестве результата возвращать Option
     */

    def option: ZIO[R, Nothing, Option[A]] =
      foldM(
        _ => ZIO(r => Right(None)),
        v => ZIO(r => Right(Some(v)))
      )



    /**
     * Реализовать метод, который будет работать с каналом ошибки
     */

    def mapError[E1](f : E => E1): ZIO[R, E1, A] =
      foldM(
        e => ZIO(_ => Left(f(e))),
        v => ZIO(_ => Right(v))
      )



  }











  sealed trait IntegrationError

  case object ConnectionFailed extends IntegrationError

  case object CommunicationFailed extends IntegrationError

  lazy val connect: IO[ConnectionFailed.type, Unit] = ???

  lazy val getSomeData: IO[CommunicationFailed.type, String] = ???


  lazy val connectAndGetData: IO[IntegrationError, String] =
    connect zipRight getSomeData


  lazy val io1: IO[String, String] = ???

  lazy val io2: IO[Int, String] = ???

  /**
   * 1. Какой будет тип на выходе, если мы скомбинируем эти два эффекта с помощью zip
   */

   lazy val z1: zio.ZIO[Any, Any, (String, String)] = io1 zip io2

  /**
   * Можем ли мы как-то избежать потерю информации об ошибке, в случае композиции?
    */

  lazy val io3: zio.ZIO[Any, Either[String, Int], (String, String)] =
    io1.mapError(Left(_)).zip(io2.mapError(Right(_)))


  def either: Either[String, Int] = ???

  def errorToErrorCode(str: String): Int = ???

  lazy val effFromEither: IO[String, Int] = zio.ZIO.fromEither(either)

  /**
   * Логировать ошибку effFromEither, не меняя ее тип и тип возвращаемого значения
   */
  lazy val z2: zio.ZIO[Any, String, Int] = effFromEither.tapError{ e =>
    zio.ZIO.attempt(println(e)).orDie
  }


  /**
   * Изменить ошибку effFromEither
   */

  lazy val z3 = ???


  // Разные типы ошибок

    type User = String
    type UserId = Int

    sealed trait NotificationError
    case object NotificationByEmailFailed extends NotificationError
    case object NotificationBySMSFailed extends NotificationError
    case object UserNotFound


    def getUserById(userId: UserId): Task[User] = ???

    def sendEmail(user: User, msg: String): IO[NotificationByEmailFailed.type, Unit] = ???

    def sendSMS(user: User, msg: String): IO[NotificationBySMSFailed.type, Unit] = ???

    def sendNotification(userId: UserId): IO[NotificationError, Unit] = for{
      user <- getUserById(1).orDie
      _ <- sendEmail(user, "msg1")
      _ <- sendSMS(user, "msg1")
    } yield ()

   def readFile(name: String): Task[List[String]] =
     zio.ZIO.fail(new NumberFormatException("Ooops"))

   lazy val z4: Task[List[String]] =
     readFile("test1.txt").refineOrDieWith{
       case e: IOException => e
     }(_ => new Throwable("Foo"))

}