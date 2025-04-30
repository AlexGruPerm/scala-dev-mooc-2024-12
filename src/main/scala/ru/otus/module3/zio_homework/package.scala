package ru.otus.module3

import ru.otus.module3.zio_homework.config.AppConfig
import zio.{Clock, Console, IO, Random, Task, UIO, ULayer, URIO, ZIO, ZLayer, durationInt}

import java.io.IOException
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */
    //class InvalidInputValue extends Exception

    private class InputError extends Exception

    private def checkIntegerValue(i: Int): Boolean =
      i == 1 || i == 2 || i == 3

    private def checkInput(str: String): ZIO[Any,InputError,Int] =
      ZIO.attempt(str.toInt).catchAll(_ => ZIO.fail(new InputError)).
        filterOrFail(checkIntegerValue )(new InputError)

   /**
    * Читает с консоли ввод и проверяет его на возможность приведения к Int, такого, что значение ввода
    * равно (1,2,3)
   */
   private val readWhileIncorrect: Task[Int] = {

     def attemptRead = for {
       _ <- Console.printLine(s"Введете целое значение от 1 до 3:")
       srcIntValue <- Console.readLine.orDie
       intValue <- checkInput(srcIntValue).map(Some(_)).catchAll { _ =>
         Console.printLine(s"Вы ввели не корректное значение [$srcIntValue] должно быть (1,2,3)").as(None)
       }
     } yield intValue

     attemptRead.repeatUntil(_.isDefined).map(_.get)
   }

   private def compareAndOutput(in: Int, rv: Int): UIO[Unit] =
     ZIO.succeed(in == rv).flatMap {
     case true  => Console.printLine(s"Вы угадали случайное число = $in").orDie
     case false => Console.printLine(s"Вы не угадали случайное число. Число = $rv ваш ввод = $in").orDie
   }

   lazy val guessProgram: Task[Unit] = for {
      rv <- Random.nextIntBetween(1, 4)
      in <- readWhileIncorrect
      _ <- compareAndOutput(in,rv)
   } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор,
   *    пока его значение в условии не даст true
   * 
   */
    def testEffectForDoWhile(in: Int): UIO[Boolean] = for {
      rv <- Random.nextIntBetween(1, 10)
      _ <- Console.printLine(s"in=$in rv=$rv").orDie
      compare <- ZIO.attempt(rv == in).orDie
    } yield compare


  def doWhile(eff: UIO[Boolean]): UIO[Unit] = for {
    r <- eff
    _ <- if (r)
      ZIO.succeed(true)
    else doWhile(eff)
  } yield ()

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */
  import config.Configuration
  val defaultConfig = AppConfig(
    host = "localhost",
    port = "8080"
  )

  def loadConfigOrDefault: UIO[AppConfig] = Configuration.config
    .catchAll(error =>
      Console.printLine(s"Ошибка загрузки конфига: ${error.getMessage}") *>
        Console.printLine("Используем дефолтный конфиг") *>
        ZIO.succeed(defaultConfig)
    )
    .tap(config => Console.printLine(s"Успешно загружен конфиг: $config"))
    .orDie

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: UIO[Int] = for {
    //random <- Random.nextIntBetween(1, 11).delay(1 second)
    random <- ZIO.sleep(1 second) *> Random.nextIntBetween(1,11)
    _ <- Console.printLine(s"random = $random").orDie
  } yield random

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[UIO[Int]] = List.fill(10)(eff)
  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */
  private lazy val sumEffects: UIO[Int] = for {
    sum <- ZIO.collectAll(effects).map(_.sum)
    _ <- Console.printLine(s"sum = $sum").orDie
  } yield sum

  def printEffectRunningTime(eff: UIO[Int]): UIO[Int] = for {
    begin <- Clock.nanoTime
    sum <- eff
    end <- Clock.nanoTime
    _ <- Console.printLine(s"duration = ${(end - begin)/1000000} ms.").orDie
  } yield sum

  lazy val app = printEffectRunningTime(sumEffects)

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */
  // выше в sumEffects сделать запуск эффектов последовательным, а тут в 4.4. //
  lazy val appSpeedUp: UIO[Int] = for {
    sum <- ZIO.collectAllPar(effects).map(_.sum)
    _ <- Console.printLine(s"sum = $sum").orDie
  } yield sum


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
  //1. Интерефейс сервиса
  trait PrintEffectRunningTime{
    def runningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A]
  }

  //2. Реализация сервиса
  private case class PrintEffectRunningTimeImpl() extends PrintEffectRunningTime {
    def runningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A] = for {
      begin <- Clock.nanoTime
      res <- eff
      end <- Clock.nanoTime
      _ <- Console.printLine(s"duration = ${(end - begin)/1000000} ms.").orDie
    } yield res
  }

  object PrintEffectRunningTime{

    val live: ULayer[PrintEffectRunningTime] = ZLayer.succeed(PrintEffectRunningTimeImpl())

    def runningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[PrintEffectRunningTime with R, E, A] =
      ZIO.serviceWithZIO[PrintEffectRunningTime](_.runningTime(eff))

  }



   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  private lazy val appWithTimeLogg: ZIO[PrintEffectRunningTime, Nothing, Int] =
    PrintEffectRunningTime.runningTime(sumEffects)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[PrintEffectRunningTime, Nothing, Int] = appWithTimeLogg

}
