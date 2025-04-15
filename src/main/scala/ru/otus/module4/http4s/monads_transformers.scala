package ru.otus.module4.http4s

import cats.data.{EitherT, OptionT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object monads_transformers {
// Future[Either[String, Option[Int]]]
// val transform = result.map {
  // case Right(Some(v)) => Right(Some(v+1))
  // }

  //1.
  def getUserName: IO[Option[String]] = IO.pure(Some("sdzf"))
  def getId(name: String): IO[Option[Int]] = IO.raiseError(new Exception("some error"))
  def getPermissions(id: Int): IO[Option[String]] = IO.pure(Some("permissions"))

  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    //1
    val res: OptionT[IO, (String, Int, String)] = for {
      username <- OptionT(getUserName)
      id <- OptionT(getId(username))
      permissions <- OptionT(getPermissions(id))
    } yield (username, id, permissions)

   // println(res.value.unsafeRunSync())

    //2.
    def getUserName1: IO[Option[String]] = IO.pure(Some("xsdg"))
    def getId1(name: String): IO[Int] = IO.pure(42)
    def getPermissions1(id:Int): IO[Option[String]] = IO.pure(Some("permissions"))
    val res1 = for {
      username <- OptionT(getUserName1)
      id <- OptionT.liftF(getId1(username))
      permissions <- OptionT(getPermissions1(id))
    } yield(username, id, permissions)

    //3. EitherT
    sealed trait UserServiceError
    case class PermissionDenied(msg: String)  extends UserServiceError
    case class UserNotFound(userId: Int) extends UserServiceError
    def getUserName2(userid: Int): EitherT[IO, UserServiceError, String] = EitherT.pure("test")
    def getUserAddress(userid:Int): EitherT[IO, UserServiceError, String]=
      EitherT.fromEither(Right("dfg"))

    def getProfile(id:Int) = for {
      name <- getUserName2(id)
      address <- getUserAddress(id)
    } yield (name, address)

  }


}
