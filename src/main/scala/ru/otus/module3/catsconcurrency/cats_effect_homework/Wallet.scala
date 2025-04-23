package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  def getWalletId: WalletId
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся,
// делаем максимально простую рабочую имплементацию.
// (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  private val filePath = Paths.get(s"$id.wallet")

  def getWalletId: WalletId = id

  def balance: F[BigDecimal] = Sync[F].delay {
    if (Files.exists(filePath)) {
      val contentString = Files.readString(filePath, StandardCharsets.UTF_8)
      BigDecimal(contentString.trim)
    } else {
      BigDecimal(0)
    }
  }

  private def saveBalanceToFile(amount: BigDecimal): F[Unit] = Sync[F].delay {
    Files.writeString(
      filePath,
      amount.toString,
      StandardCharsets.UTF_8,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE
    )
    ()
  }

  def topup(amount: BigDecimal): F[Unit] = balance.flatMap { current =>
    saveBalanceToFile(current + amount)
  }

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] =
    balance.flatMap { current =>
      if (current >= amount) {
        saveBalanceToFile(current - amount).map(_ => Right(()))
      } else {
        Sync[F].pure(Left(BalanceTooLow))
      }
    }

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов

  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =
    Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
