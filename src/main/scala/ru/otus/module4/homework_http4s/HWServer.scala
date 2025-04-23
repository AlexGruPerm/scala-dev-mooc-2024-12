package ru.otus.module4.homework_http4s

import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.comcast.ip4s.{Host, Port}
import io.circe.syntax.EncoderOps
import org.http4s.{Http, HttpRoutes, Request, Response}
import org.http4s.dsl.io.{->, /, GET, Ok, POST, Root}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import io.circe.generic.auto._
import org.http4s.implicits._
import org.http4s.circe._
import fs2.{Chunk, Stream}
import io.circe.parser
import org.http4s.client.Client

import scala.concurrent.duration._
import scala.util.Random


object HWServer {

  case class CounterResponse(counter: Int)

  case class SlowParams(chunk: Int, total: Int, time: Int)

  def StrToInt(in: String): Option[Int] = in.toIntOption

  def responseStream(params: SlowParams): Stream[IO, Byte] = {

    def randomIntAsBytes: Array[Byte] = Random.nextInt().toString.getBytes

      Stream
        .repeatEval(IO(randomIntAsBytes)) // данные
        .flatMap { bytes =>
          val chunk = Chunk.array(bytes.take(params.chunk))
          Stream.chunk(chunk) ++
            Stream.sleep_[IO](params.time.seconds) // Пауза
        }
        .take(params.total)
  }

  /**
   chunk - кусками по N байт,
   пока не не достигнет total байт.
   каждые time секунд
  */
  def SlowResponse(chunk: String, total: String, time: String): IO[Response[IO]] =
    (StrToInt(chunk),StrToInt(total),StrToInt(time)) match {
      case (Some(ch),Some(tot),Some(tm)) if ch>0 && tot>0 && tm>0 =>
        val params = SlowParams(ch,tot,tm)
        //Ok(s"params = $params")
        Ok(responseStream(params))
      case _ => BadRequest(s"Invalid request data")
    }

  def routes(counter: Ref[IO, Int]): HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "counter" =>
        counter.modify(c => (c + 1, c + 1)).flatMap { newValue =>
          Ok(CounterResponse(newValue).asJson)
        }
      case GET -> Root / "slow"/ chunk / total / time => SlowResponse(chunk, total, time)
    }

  val server = for {
    counterRef <- Ref.of[IO, Int](0)
    srv <-EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routes(counterRef).orNotFound)
      .build
      .use { server =>
        IO.println(s"Server started at ${server.address}") *>
          IO.never
      }
  } yield srv

}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    HWServer.server
  }
}
/**
 *
 для http://localhost:8080/counter
  возвращает {"counter":3} с каждым вызовом счетчик увеличивается

 для http://localhost:8080/slow/10/30/1
  возврат 337683534152277788057506146972 спутся 3 секунды

*/

object SimpleServerTest extends IOApp.Simple {

  def run(): IO[Unit] =
    runTestsCounter() *>
    testSlow()

  /** Вывод:
   Counter test: 1 (should be 1)

   Статус: 200 OK
   Общее время потока: ~3 сек (ожидаем ~3 сек)
   */

  def runTestsCounter(): IO[Unit] = {
    // Инициализация
    for {
      counterRef <- Ref.of[IO, Int](0)
      client = Client.fromHttpApp(HWServer.routes(counterRef).orNotFound)

      // Тест 1: Проверка /counter
      _ <- {
        val request = Request[IO](GET, uri"/counter")
        client.run(request).use { resp =>
          for {
            body <- resp.as[String]
            json <- IO.fromEither(parser.parse(body))
            cnt <- IO(json.hcursor.get[Int]("counter").getOrElse(-1))
            _ <- IO(println(s"Counter test: $cnt (should be 1)"))
          } yield ()
        }
      }
    } yield ()
  }

  def testSlow(): IO[Unit] = {
    // 1. Инициализация
    Ref.of[IO, Int](0).flatMap { counterRef =>
      val client = Client.fromHttpApp(HWServer.routes(counterRef).orNotFound)
      val request = Request[IO](GET, uri"/slow/10/30/1")

      // 2. Запуск запроса и проверка
      client.run(request).use { response =>
        for {
          _ <- IO(println(s"Статус: ${response.status}")) // Ожидается 200 OK
          // 3. Чтение первых 30 байт (3 чанка по 10)
          _ <- response.body
            .take(30)
            .compile
            .toVector
          // 4. Проверка времени
          startTime <- IO.realTime
          _ <- response.body.compile.drain
          endTime <- IO.realTime
          duration = (endTime - startTime).toSeconds
          _ <- IO(println(s"Общее время потока: ~${duration} сек (ожидаем ~3 сек)"))
        } yield ()
      }
    }
  }

}