package ru.otus.module3

import zio.{Console, UIO, ULayer, URIO, ZIO, ZLayer}


package object emailService {

    /**
     * Реализовать Сервис с одним методом sendEmail,
     * который будет принимать Email и отправлять его
     */

     trait EmailService{
        def sendMail(email: Email): UIO[Unit]
     }

     object EmailService{
       case class EmailServiceImpl(console: Console) extends EmailService{
         override def sendMail(email: Email): UIO[Unit] =
           console.printLine(email).orDie
       }

       def sendMail(email: Email): URIO[EmailService, Unit] =
         ZIO.serviceWithZIO[EmailService](_.sendMail(email))

       val live: ZLayer[Console, Nothing, EmailService] =
         ZLayer.fromFunction(c => EmailServiceImpl(c))
     }




}
