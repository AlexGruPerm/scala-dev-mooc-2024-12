package ru.otus.module4.http4smiddleware

import cats.Functor
import cats.data.{Kleisli, OptionT}
import cats.effect._
import cats.implicits.toSemigroupKOps
import com.comcast.ip4s.{Host, Port}
import org.http4s.{AuthedRequest, AuthedRoutes, Http, HttpRoutes, Method, Request, Response, Status, Uri}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.{AuthMiddleware, HttpMiddleware, Router}
import org.typelevel.ci.CIStringSyntax

class catsmiddleware {
  //1
  /*
  Kleisli[F[_], A, B ] A => F[B]
  Reader[R,A] ~ Kleisli[Id, R, A] ~ R=>A
  ReaderT[F, R, A] = Kleisli[F,R,A] = R=>F[A]

  type HttpRoutes[F[_]] = Kleisli[OptionT[F,*], Request[F], Response[F]]
  Request[F] => OptionT[F, Response[F]]


  val helloRoute: mHttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" =>
      Ok("hello")
  }
  ~
  val helloRoute: Kleisli[OptionT[IO, *], Request[IO], Response[IO]] = Kleisli { request =>
    OptionT.pure[IO](Response[IO](status = Status.Ok)).withEntity("hello")
  }
*/
  /*
  trait UserService {
    def getUser(id: String): IO[String]
  }

  type AppEnv = UserService
  type App[A] = ReaderT[IO, AppEnv, A]

  def userRoutes: HttpRoutes[App] = HttpRoutes.of[App] {
    case GET -> Root / "user" / id =>
      for {
        env <- ReaderT.ask[IO, AppEnv]
        user <- ReaderT.liftF(env.getUser(id))
        resp <- ReaderT.liftF(Ok(user))
      } yield resp
  }
  */
}



object Restfull {

  //1
  val service: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root / "hello"/name => Ok(s"bla bla bla")
  }

  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello1" / name => Ok(s"bla1 bla1 bla1")
    }

  val serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello2" / name => Ok("bla2 bla2 bla2")
    }

  val router = addResponseMiddleware(Router(
    "/" -> addResponseMiddleware(serviceOne),
    "/api" -> addResponseMiddleware(serviceTwo),
    "/apiroot" -> addResponseMiddleware(service)
  ))

  //2
  def addResponseMiddleware[F[_]: Functor](
                                          routes: HttpRoutes[F]
                                          ): HttpRoutes[F] = Kleisli{
    req =>
      val maybeResponse = routes(req)
      maybeResponse.map {
        case Status.Successful(resp) => resp.putHeaders("X-Otus"-> "Hello")
        case other => other
      }
  }

  // 3 sessions

  type Sessions[F[_]] = Ref[F, Set[String]]
  def serviceSessions(sessions: Sessions[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case r@GET -> Root / "hello" =>
        r.headers.get(ci"X-User-Name") match {
          case Some(values) =>
            val name = values.head.value
            sessions.get.flatMap(users =>
            if (users.contains(name)) Ok(s"Hello, $name")
            else
              Forbidden("you shell not pass!!!")
            )
          case None => Forbidden("you shell not pass!!!")
        }

      case PUT -> Root / "login" / name =>
        sessions.update(set => set + name).flatMap(_=>Ok("done"))
    }

  //Auth
  def serviceAuth(sessions: Sessions[IO]): HttpMiddleware[IO] =
    routes =>
    Kleisli{req =>
      req.headers.get(ci"X-User-Name") match {
        case Some(values) =>
          val name = values.head.value
          for {
            users <- OptionT.liftF(sessions.get)
            results <-
              if (users.contains(name)) routes(req)
              else OptionT.liftF(Forbidden("You shell not pass!!!"))
          } yield results
        case None => OptionT.liftF(Forbidden("You shell not pass!!!"))
      }
    }

  def routerSessionsAuth(sessions: Sessions[IO]) =
    addResponseMiddleware(Router("/" -> (loginService(sessions) <+> serviceAuth(sessions)(serviceHello))))

  def loginService(sessions: Sessions[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case PUT -> Root / "login" / name =>
        sessions.update(set => set+name).flatMap(_=>Ok("done"))
    }

  def serviceHello: HttpRoutes[IO] =
    HttpRoutes.of{
      case r@GET -> Root / "hello" =>
        r.headers.get(ci"X-User-Name") match {
          case Some(values) =>
            val name = values.head.value
            Ok(s"Hello, $name")
          case None => Forbidden("you shell not pass!!!")
        }
    }


  def routerSessions(sessions: Sessions[IO]) =
    addResponseMiddleware(Router("/" -> serviceSessions(sessions)))

  val server1 = for {
    s<-EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router.orNotFound).build
  } yield s

  val serverSessions = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s<-EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessions(sessions).orNotFound).build
  } yield s

  val serverSessionsAuth = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s<-EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionsAuth(sessions).orNotFound).build
  } yield s

  //refactoring
  final case class User(name: String)
  def serviceHelloAuth: AuthedRoutes[User, IO] = AuthedRoutes.of {
    case GET -> Root / "hello" as user =>
      Ok(s"Hello, ${user.name}")
  }

  def serviceAuthMiddleware(sessions: Sessions[IO]): AuthMiddleware[IO, User] =
    authedRoutes =>
      Kleisli {req =>
        req.headers.get(ci"X-User-Name") match {
          case Some(values) =>
            val name = values.head.value

            for {
              users <- OptionT.liftF(sessions.get)
              results <-
                if (users.contains(name)) authedRoutes(AuthedRequest(User(name), req))
                else OptionT.liftF(Forbidden("you shell not pass!!!"))
            } yield results
          case None => OptionT.liftF(Forbidden("you shell not pass!!!"))
        }
      }

  def routerSessionsAuthClear(sessions: Sessions[IO]) =
    addResponseMiddleware(Router("/" -> (loginService(sessions) <+> serviceAuthMiddleware(sessions)(serviceHelloAuth))))

  val serverSessionsAuthClear = for {
    sessions <- Resource.eval(Ref.of[IO, Set[String]](Set.empty))
    s<-EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routerSessionsAuthClear(sessions).orNotFound).build
  } yield s
}



object mainService extends IOApp.Simple {
  def run(): IO[Unit] = {
    //Restfull.server1.use(_ => IO.never)
//    Restfull.serverSessions.use(_ => IO.never)
//    Restfull.serverSessionsAuth.use(_ => IO.never)
    Restfull.serverSessionsAuthClear.use(_ => IO.never)
  }
}

//6 tests
/*
object Test extends IOApp.Simple {
  def run: IO[Unit] = {
    val service = Restfull.serviceHelloAuth

    for {
      result <- service(AuthedRequest(Restfull.User("test"), Request(method = Method.GET,
        uri = Uri.fromString("/hello").toOption.get))).value
      res <- result match {
        case Some(resp) =>
           ???
        case None => ???
      }
    } yield res
  }
}*/