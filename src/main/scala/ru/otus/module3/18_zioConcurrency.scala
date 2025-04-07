package ru.otus.module3

import zio.{Clock, Console, Executor, Ref, UIO, URIO, ZIO, durationInt}

import java.io.IOException
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.language.postfixOps


object zioConcurrency {


  // эффект содержит в себе текущее время
  val currentTime: URIO[Clock, Long] = Clock.currentTime(TimeUnit.SECONDS)


  /**
   * Напишите эффект, который будет считать время выполнения любого эффекта
   */


    // 1. Получить время
    // 2. выполнить эффект
    // 3. получить время
    // 4. вывести разницу
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for{
      start <- currentTime
      r <- zio
      end <- currentTime
      _ <- Console.printLine(s"Running time ${end - start}").orDie
    } yield r


  val exchangeRates: Map[String, Double] = Map(
    "usd" -> 76.02,
    "eur" -> 91.27
  )

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  lazy val sleep1Second = ZIO.sleep(1 seconds)

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 3 секунды
   */
  lazy val sleep3Seconds = ZIO.sleep(3 seconds)

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation1 спустя 3 секунды
   */
  lazy val getExchangeRatesLocation1: ZIO[Any, IOException, Int] =
    sleep3Seconds zipRight Console.printLine("GetExchangeRatesLocation1") zipRight ZIO.succeed(10)

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation2 спустя 1 секунду
   */
  lazy val getExchangeRatesLocation2 =
    sleep1Second zipRight Console.printLine("GetExchangeRatesLocation2") zipRight ZIO.succeed(20)


  /**
   * Написать эффект который получит курсы из обеих локаций
   */

   lazy val getFrom2Locations: ZIO[Any, IOException, (Int, Int)] =
     getExchangeRatesLocation1 zip getExchangeRatesLocation2


  /**
   * Написать эффект который получит курсы из обеих локаций параллельно
   */

  lazy val getFrom2LocationsPar = for{
    f1 <- getExchangeRatesLocation1.fork
    f2 <- getExchangeRatesLocation2.fork
    r1 <- f1.join
    r2 <- f2.join
  }yield (r1, r2)


  /**
   * Предположим нам не нужны результаты, мы сохраняем в базу и отправляем почту
   */


   lazy val writeUserToDB =
     sleep3Seconds zipRight Console.printLine("writeUserToDB")

   lazy val sendMail = sleep1Second zipRight Console.printLine("sendMail")

  /**
   * Написать эффект который сохранит в базу и отправит почту параллельно
   */

  lazy val writeAndSend = for{
    _ <- writeUserToDB.fork
    _ <- sendMail.fork
  } yield ()


  /**
   *  Greeter
   */

  lazy val greeter: ZIO[Any, IOException, Nothing] = (sleep1Second zipRight zio.Console.printLine("Hello")) zipRight greeter

  def imperativeGreeter(cancelled: AtomicBoolean) = {
    while(!cancelled.get()){
      println("Hello")
    }
  }
  lazy val g1 = for{
    ref <- ZIO.succeed(new AtomicBoolean(false))
    f <- ZIO.attemptBlockingCancelable(imperativeGreeter(ref))(
      ZIO.succeed(ref.set(true))).fork
    _ <- sleep3Seconds
    _ <- f.interrupt
    _ <- ZIO.sleep(2 seconds)
  } yield ()





  /***
   * Greeter 2
   * 
   * 
   * 
   */


 lazy val greeter2 = ???
  

  /**
   * Прерывание эффекта
   */

   lazy val app3 = ???





  /**
   * Получение информации от сервиса занимает 1 секунду
   */
  def getFromService(ref: Ref[Int]) = ???

  /**
   * Отправка в БД занимает в общем 5 секунд
   */
  def sendToDB(ref: Ref[Int]): ZIO[Clock, Exception, Unit] = ???


  /**
   * Написать программу, которая конкурентно вызывает выше описанные сервисы
   * и при этом обеспечивает сквозную нумерацию вызовов
   */

  
  lazy val app1 = ???

  /**
   *  Concurrent operators
   */

  lazy val a = ZIO.sleep(1 seconds) zipRight ZIO.succeed(10)
  lazy val b = ZIO.sleep(3 seconds) zipRight ZIO.succeed(20)

  lazy val p1: ZIO[Any, Nothing, (Int, Int)] = a zipPar b
  lazy val p2: ZIO[Any, Nothing, Either[Int, Int]] = a raceEither b
  lazy val p3: ZIO[Any, Nothing, Int] = a race b

  lazy val p4 = ZIO.foreachPar(List(1, 2, 3, 4, 5))( i =>
    sleep1Second zipRight Console.printLine(i)
  )


  /**
   * Lock
   */


  // Правило 1
  lazy val doSomething: UIO[Unit] = ???
  lazy val doSomethingElse: UIO[Unit] = ???

  lazy val executor: Executor = ???

  lazy val eff = for{
    f1 <- doSomething.fork
    _ <- doSomethingElse
    r <- f1.join
  } yield r

  lazy val result = eff.onExecutor(executor)



  // Правило 2
  lazy val executor1: Executor = ???
  lazy val executor2: Executor = ???



  lazy val eff2 = for{
      f1 <- doSomething.onExecutor(executor2).fork
      _ <- doSomethingElse
      r <- f1.join
    } yield r

  lazy val result2 = eff2.onExecutor(executor)



}