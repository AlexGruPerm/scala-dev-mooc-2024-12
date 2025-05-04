package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    private val dc = db.Ctx
    import dc._

/*    private val userSchema = quote {
        querySchema[User]("Users")  // Теперь таблица будет называться "users" вместо "User"
    }*/

    private val userSchema = quote{
        querySchema[User]("users")
    }

    private val UserToRoleSchema = quote{
        querySchema[UserToRole]("usertorole")
    }

    private val RoleSchema = quote{
        querySchema[Role]("roles")
    }

    override def findUser(userId: UserId): QIO[Option[User]] =
        dc.run(userSchema.filter(_.id == lift(userId.id)).take(1)).map(_.headOption)

    override def createUser(user: User): QIO[User] =
        dc.run(userSchema.insertValue(lift(user)).returning(insertedRow => insertedRow))

    override def createUsers(users: List[User]): QIO[List[User]] =
        dc.run(liftQuery(users).foreach(u => userSchema.insertValue(u).returning(insertedRow => insertedRow)))
    //dc.run(liftQuery(users).foreach(u => userSchema.insertValue(u))).as(users) ???

    override def updateUser(user: User): QIO[Unit] =
        dc.run(userSchema.filter(_.id == lift(user.id)).updateValue(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] =
        dc.run(userSchema.filter(_.id == lift(user.id)).delete).unit

    override def findByLastName(lastName: String): QIO[List[User]] =
        dc.run(userSchema.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] =
        dc.run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] =
        dc.run(
            UserToRoleSchema.filter(_.userId == lift(userId.id))
              .join(RoleSchema)
              .on(_.roleId == _.roleId)
              .map(_._2))

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] =
       for {
        optRole <- findRoleByCode(roleCode)
        _ <- optRole match {
            case Some(role) => dc.run(UserToRoleSchema.insertValue(lift(UserToRole(role.roleId, userId.id)))).unit
            case None => ZIO.unit
        }
    } yield ()

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] =
        dc.run(
            RoleSchema.filter(_.code == lift(roleCode.code))
              .join(UserToRoleSchema)
              .on(_.roleId == _.roleId)
              .join(userSchema)
              .on(_._2.userId == _.id)
              .map(_._2)
        )

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] =
        dc.run(RoleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)
}

object UserRepository{

    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}