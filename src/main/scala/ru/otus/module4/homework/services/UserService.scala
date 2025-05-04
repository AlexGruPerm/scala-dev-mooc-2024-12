package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.logging.LogAnnotation.UserId
import zio.{ZIO, ZLayer}

import java.sql.SQLException

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
    val dc = db.Ctx

    def listUsers(): QIO[List[User]] =
        userRepo.list()

    def listUsersDTO(): QIO[List[UserDTO]] =
        for {
        users <- listUsers()
        userRoles <- ZIO.foreach(users){
            u => userRepo.userRoles(u.typedId).map(_.toSet).map(UserDTO(u,_))
          }
        } yield userRoles

    /**
     * 1) Ищем роль в справчонике findRoleByCode, если роли нет, то (ни чего не делаем) выкидываем исключение
     *    (т.к. у нас не предусмотрено добавление роли createRole).
     * 2) Если роль нашли, то
     * 3) Ищем юзера findUser:
     *    3.1) если находим, то обновляем его (обновляем т.к. входной параметр user: User, а не просто userId).
     *         Далее по этому пользователю достаем все его роли userRoles, если среди его ролей уже есть roleCode:
     *           3.1.1) то ни чего не делаем (связка уже есть).
     *           3.1.2) иначе, делаем вставку в таблицу связки insertRoleToUser
     *    3.2) если не нашли пользователя, то добавляем createUser (на выходе получаем юзера с сгенерированным userId)
     *         и создаем связку с ролью (из п.2.)
     *
    */
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = for {
      optRole <- userRepo.findRoleByCode(roleCode)
      //role <- optRole.fold(ZIO.fail(new SQLException(s"Role with code ${roleCode.code} not found.")))(ZIO.succeed(_))
      role <- ZIO.fromOption(optRole).orElseFail(new SQLException(s"Role with code ${roleCode.code} not found."))
      userFromDb <- userRepo.findUser(user.typedId)
      userForUse <- userFromDb.fold(userRepo.createUser(user))(u => userRepo.updateUser(user).as(u))
      userRolesInDb <- userFromDb.fold(QIO.apply(List.empty[Role]))(u => userRepo.userRoles(u.typedId))
      _ <- userRepo.insertRoleToUser(roleCode, userForUse.typedId).when(!userRolesInDb.map(_.code).contains(roleCode))
        result <- ZIO.succeed(UserDTO(userForUse,(userRolesInDb :+ role).toSet))
    } yield result

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] =
        listUsersDTO().map(ur => ur.filter(_.roles.map(_.code).contains(roleCode.code)))

}

object UserService{

    val layer: ZLayer[UserRepository, Nothing, UserService] = ZLayer.fromFunction(new Impl(_))
}

case class UserDTO(user: User, roles: Set[Role])