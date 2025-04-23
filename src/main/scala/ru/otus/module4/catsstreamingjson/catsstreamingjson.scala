package ru.otus.module4.catsstreamingjson

import cats.effect
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource, SyncIO}
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global
import org.http4s.client.Client
import org.http4s.{Request, Response, Uri}
import org.http4s.ember.client.EmberClientBuilder
import ru.otus.module4.http4smiddleware.Restfull

import scala.concurrent.duration._
import java.time.Instant


object HttpClient {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request = Request[IO](uri = Uri.fromString("http://localhost:8080/hello").toOption.get)

  //1
  /*val result = for {
    client <- builder
    response <- client.run(request)
  } yield response
  */
  //2
  /*
  val result = for {
    client <- builder
    response <- effect.Resource.eval(client.expect[String](request))
  } yield response
   */
  //3
  val result = builder.use(client => client.run(request).use(
    resp => if (!resp.status.isSuccess)
      resp.as[String]
    else
      IO("bla bla bla")
  ))

}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    //1 and 2
/*    for {
      fiber <- Restfull.serverSessionsAuthClear.use(_ => IO.never).start
      _ <- HttpClient.result.use(IO.println)
    } yield ()
    */
    //3
    for {
      _ <- Restfull.serverSessionsAuthClear.use(_ => HttpClient.result.flatMap(IO.println) *> IO.never)
    } yield ()
  }
}