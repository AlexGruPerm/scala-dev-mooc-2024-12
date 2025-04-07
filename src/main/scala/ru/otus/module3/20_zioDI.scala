package ru.otus.module3

import zio.{Clock, Console, Duration, IO, RIO, Random, Scope, Tag, Task, UIO, URIO, ZEnvironment, ZIO, durationInt}

import scala.language.postfixOps
import java.io.IOException

object di {

  type Query[_]
  type DBError
  type QueryResult[_]
  type Email = String

  trait User{
    def email: String
  }


  trait DBService{
    def tx[T](query: Query[T]): IO[DBError, QueryResult[T]]
  }

  trait EmailService{
    def makeEmail(email: String, body: String): Task[Email]
    def sendEmail(email: Email): Task[Unit]
  }

  trait LoggingService{
    def log(str: String): Task[Unit]
  }

  trait UserService{
      def getUserBy(id: Int): RIO[LoggingService, User]
      def id: Int
  }




  /**
   * Написать эффект который напечатает в консоль приветствие, подождет 5 секунд,
   * сгенерит рандомное число, напечатает его в консоль
   *   Console
   *   Clock
   *   Random
   */

  trait Console{
    def printLine(v: Any): UIO[Unit]
  }

  trait Clock{
    def sleep(duration: Duration): UIO[Unit]
  }

  trait Random{
    def nextInt(): UIO[Int]
  }

  val e: ZIO[Random with Clock with Console, Nothing, Unit] = for{
    console <- ZIO.service[Console]
    clock <- ZIO.service[Clock]
    random <- ZIO.service[Random]
    _ <- console.printLine("Hello")
    _ <- clock.sleep(5 seconds)
    int <- random.nextInt()
    _ <- console.printLine(int)
  } yield ()


 type MyEnv = Random with Clock with Console

  val e2: ZIO[MyEnv, Nothing, Unit] = e

  // def identity[A](a: A): A = a
  // def serviceWithZIO[A, B, E, R](f: A => ZIO[R, E, B]): ZIO[A with R, E, B]
  // def serviceWith[A, B, E, R](f: A => B): ZIO[A, E, B]
  // def service[A]: ZIO[A, E, A] = serviceWith(identity)


  lazy val getUser: ZIO[LoggingService with UserService, Throwable, User] = {
    ZIO.serviceWithZIO[UserService](us => us.getUserBy(10))
  }


  lazy   val e3: URIO[Int, Unit] = ???
  lazy   val e4: URIO[String, Unit] = ???

  /**
   * Эффект, который будет комбинацией двух эффектов выше
   */

  lazy val e5: ZIO[String with Int, Nothing, Unit] = e3 zip e4



  /**
   * Написать ZIO программу которая выполнит запрос и отправит email
   */




  lazy val services: ZEnvironment[UserService with EmailService with LoggingService] = ???

  lazy val dBService: DBService = ???
  lazy val userService: UserService = ???

  lazy val emailService2: EmailService = ???

  def f(userService: ZEnvironment[UserService]): ZEnvironment[UserService with EmailService with LoggingService] = ???


  lazy   val queryAndNotify: ZIO[LoggingService with EmailService with UserService, Throwable, Unit] = ???
    // provide

  lazy  val e6: IO[Throwable, Unit] =
      queryAndNotify.provideEnvironment(services)

  // provide some

  lazy  val e7: ZIO[UserService, Throwable, Unit] =
    queryAndNotify.provideSomeEnvironment[UserService](f)


  trait Context{
    def close: UIO[Unit]
    def addFinalizer[A](f: => UIO[Any]): UIO[Unit]
  }

  object Context{
    def withFinalizer[R, E, A](zio: ZIO[R, E, A])(finalizer: A => UIO[Any]): ZIO[R with Context, E, A] = {
      zio.flatMap{ a =>
        ZIO.serviceWithZIO[Context](_.addFinalizer(finalizer(a))) *> ZIO.succeed(a)
      }
    }
    private def f[R: Tag](e: ZEnvironment[R]): ZEnvironment[R with Context] = {
      val c = new Context {
        val finalizers = scala.collection.mutable.ListBuffer.empty[UIO[Any]]

        override def close: UIO[Unit] = ZIO.collectAll(finalizers.toList).unit

        override def addFinalizer[A](f: => UIO[Any]): UIO[Unit] =
          ZIO.succeed(finalizers.addOne(f))
      }
      ZEnvironment(c).++[R](e)
    }


    def inContext[R: Tag, E, A](zio: ZIO[R with Context, E, A]): ZIO[R, E, A] = {
      zio.flatMap{ a =>
        ZIO.serviceWithZIO[Context](_.close) *> ZIO.succeed(a)
      }.provideSomeEnvironment[R](zr => f[R](zr))
    }
  }

  val cc: ZIO[Context, IOException, Unit] = Context.withFinalizer(zio.Console.printLine("Hello world 1"))(_ =>
    zio.Console.printLine("Running finalizer 1").orDie)

  val cc2: ZIO[Context, IOException, Unit] = Context.withFinalizer(zio.Console.printLine("Hello world 2"))(_ =>
    zio.Console.printLine("Running finalizer 2").orDie)

  val cc3: ZIO[Any, IOException, Unit] = Context.inContext[Any, IOException, Unit](cc *> cc2)




}