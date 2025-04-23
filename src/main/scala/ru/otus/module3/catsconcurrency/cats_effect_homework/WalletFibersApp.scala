package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.Stream
import ru.otus.module3.catsconcurrency.cats_effect_homework.Wallet.WalletId

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())

object WalletFibersApp extends IOApp.Simple {

  def populatLoop(wallet: Wallet[IO], interval: FiniteDuration): IO[Unit] = {
    Stream
      .repeatEval {
        wallet.topup(100.0)
      }
      .metered(interval) // пауза
      .compile
      .drain
  }

  private def printBalances(wallets: Seq[(WalletId,Wallet[IO])]): IO[Unit] =
    Stream
      .repeatEval {
        wallets.traverse_ { w =>
          w._2.balance.flatMap(b => IO.println(s"Wallet [${w._1}] balance = $b"))
        } >> IO.sleep(1.second)
      }
      .compile
      .drain

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")

      // Запускаем все процессы в фоне
      fiber1 <- populatLoop(wallet1, 100.milliseconds).start
      fiber2 <- populatLoop(wallet2, 500.milliseconds).start
      fiber3 <- populatLoop(wallet3, 2000.milliseconds).start

      outputFiber <- printBalances(Seq(("1",wallet1),("2",wallet2),("3",wallet3))).start

      _ <- IO.readLine
      _ <- (fiber1.cancel, fiber2.cancel, fiber3.cancel, outputFiber.cancel).parTupled

      _ <- IO.println("Пополнения кошельков остановлены.")
    } yield ()

  /** Пример вывода:
   Press any key to stop...
   Wallet [1] balance = 24200.0
   Wallet [2] balance = 4800.0
   Wallet [3] balance = 1000.0
   Wallet [1] balance = 25100.0
   Wallet [2] balance = 4900.0
   Wallet [3] balance = 1000.0
   Wallet [1] balance = 26200.0
   Wallet [2] balance = 5200.0
   Wallet [3] balance = 1100.0

   Пополнения кошельков остановлены.
   */

}
