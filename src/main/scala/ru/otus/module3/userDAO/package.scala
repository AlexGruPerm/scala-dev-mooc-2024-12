package ru.otus.module3

import ru.otus.module3.emailService.EmailAddress
import ru.otus.module3.userService.User
import ru.otus.module3.userService.UserID
import zio.{Task, ULayer, ZIO, ZLayer}


package object userDAO {

  /**
   * Реализовать сервис с двумя методами
   *  1. list - список всех пользователей
   *  2. findBy - поиск по User ID
   */

  trait UserDAO{
    def list(): Task[List[User]]
    def findBy(id: UserID): Task[Option[User]]
  }


  object UserDAO{
    class UserDAOImpl extends UserDAO{
      private val users = ZIO.attempt(List(User(UserID(1), EmailAddress("foo@mail.com"))))
      override def list(): Task[List[User]] = users

      override def findBy(id: UserID): Task[Option[User]] =
        users.map(_.find(_.id == id))
    }

    val live: ULayer[UserDAO] = ZLayer.succeed(new UserDAOImpl)
  }



}
