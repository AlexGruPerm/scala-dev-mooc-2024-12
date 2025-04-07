package ru.otus.module3

import ru.otus.module3.emailService.{Email, EmailService, Html}
import ru.otus.module3.userDAO.UserDAO
import zio.{Console, RIO, UIO, URIO, URLayer, ZIO, ZLayer}

package object userService {

  /**
   * Реализовать сервис с одним методом
   * notifyUser, принимает id пользователя в качестве аргумента и шлет ему уведомление
   * при реализации использовать UserDAO и EmailService
   */

   trait UserService{
    def notifyUser(userId: UserID): UIO[Unit]
  }

  object UserService{
    case class UserServiceImpl(userDAO: UserDAO, emailService: EmailService) extends UserService{
      override def notifyUser(userId: UserID): UIO[Unit] = (for {
        user <- userDAO.findBy(userId).some
        email = Email(user.email, Html("Hello here"))
        _ <- emailService.sendMail(email)
      } yield ()).orElseFail(new Throwable("User not found")).orDie
    }

    def notifyUser(userId: UserID): URIO[UserService, Unit] =
      ZIO.serviceWithZIO[UserService](_.notifyUser(userId))

    val live: ZLayer[EmailService with UserDAO, Nothing, UserService] = ZLayer(
      for{
        userDAO <- ZIO.service[UserDAO]
        emailService <- ZIO.service[EmailService]
        userService <- ZIO.succeed(UserServiceImpl(userDAO, emailService))
      } yield userService
    )
  }


}
