package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    val futList: Future[List[Any]] = Future.sequence(futures.map{currFut =>
      currFut.transform{
        case Success(a) => Success(a)
        case Failure(ex) => Success(ex)
      }
    })

    val tupleResults: Future[(List[A], List[Throwable])] =
      futList.map{e =>
        e.foldLeft((List.empty[A], List.empty[Throwable])){
          case (foldRes, currRes) =>
            currRes match {
              case e: Throwable => (foldRes._1, foldRes._2 :+ e)
              case a: A => (foldRes._1 :+ a, foldRes._2)
            }
        }
      }

    tupleResults
  }

}
