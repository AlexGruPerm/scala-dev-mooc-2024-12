package collections

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = text.zipWithIndex.collect{
    case pair if pair._2 == 0 => pair._1
    case pair if isASCIIString(pair._1) => pair._1.toUpperCase
    case pair if !isASCIIString(pair._1) => pair._1.toLowerCase
  }

  /**
   * Версия с использованием foldLeft
   */
  def capitalizeIgnoringASCII2(text: List[String]): List[String] =
    if (text.nonEmpty)
      text.tail.foldLeft(List(text.head)){
        case (r, c) =>
          r :+
            (if (isASCIIString(c))
              c.toUpperCase
            else
              c.toLowerCase)
      }
    else Nil

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val dict: Map[String, String] = Map(
      "0" -> "zero",
      "1" -> "one",
      "2" -> "two",
      "3" -> "three",
      "4" -> "four",
      "5" -> "five",
      "6" -> "six",
      "7" -> "seven",
      "8" -> "eight",
      "9" -> "nine"
    )
    dict.foldLeft(text){
      case (r,(replaceWhat, replaceTo)) =>
        r.replaceAll(replaceWhat,replaceTo)
    }
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  import scala.annotation.tailrec
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Set[Auto] = {
    @tailrec
    def recurs(acc: Set[Auto] = Set.empty[Auto], dealer: Iterator[Auto]): Set[Auto] = {
      if (dealer.hasNext)
        recurs(acc + dealer.next(),dealer)
      else
        acc
    }
    recurs(recurs(dealer = dealerOne.iterator),dealer = dealerTwo.iterator)
  }
  /**
   * Так же можно к примеру через toSet, или материализацию в List + foldLeft.
   * Но, на сколько понимаю, для больших входных значений эти варианты могут проигрывать по памяти
   * и/или времени выполнения???
   */

  // Через toSet
  def intersectionAuto1(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Set[Auto] = {
    dealerOne.toSet ++ dealerTwo.toSet
  }

  //Через foldLeft
  def intersectionAuto2(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Set[Auto] = {
    (dealerOne.toList ++ dealerTwo.toList).foldLeft(Set.empty[Auto]){
      case (r,c) => r + c
    }
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   *
   * Использовал именно итерации по Iterator, т.к.:
   * Итерация по Iterator с использованием методов hasNext и next() не создает новые коллекции.
   * Преимущества:
   *   Эффективно по памяти, так как вы не создаете новые коллекции.
   *   Подходит для работы с большими объемами данных, когда вы хотите обрабатывать элементы по одному.
   *  Если вы используете методы, такие как toList, toArray, или другие методы преобразования,
   *  вы фактически создаете новую коллекцию.
   */
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    @tailrec
    def isInSecond(dealer: Iterator[Auto], auto: Auto): Boolean = {
      if (dealer.hasNext)
        if (dealer.next() == auto)
          true
        else
          isInSecond(dealer,auto)
      else
        false
    }

    @tailrec
    def onlyInFirst(acc: Set[Auto] = Set.empty[Auto], dealer: Iterator[Auto]): Set[Auto] = {
      if (dealer.hasNext){
        val auto = dealer.next()
        if (isInSecond(dealerTwo.iterator,auto))
          onlyInFirst(acc,dealer)
        else
          onlyInFirst(acc + auto,dealer)
      } else
        acc
    }

    onlyInFirst(dealer = dealerOne.iterator)
  }
}