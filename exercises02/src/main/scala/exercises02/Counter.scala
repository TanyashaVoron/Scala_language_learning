package exercises02

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] =
    text
      .split("[\\s.,!?:\\n\\t\\r()]")
      .filter(_ != "")
      .map(a => a.toLowerCase)
      .foldLeft(Map.empty[String, Int].withDefaultValue(0)) { case (count, a) => count.updated(a, count(a) + 1) }

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] =
    text
      .split("[А-Яа-я\\s.,!?:\\n\\t\\r()]")
      .filter(_ != "")
      .map(a => a.toLowerCase)
      .foldLeft(Map.empty[String, Int].withDefaultValue(0)) { case (count, a) => count.updated(a, count(a) + 1) }

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] =
    text
      .split("[А-Яа-яA-Za-z\\s!?:\\-\\n\\t\\r()]")
      .filter(_ != "")
      .filter(_ != ",")
      .foldLeft(Map.empty[String, Int].withDefaultValue(0)) { case (count, a) => count.updated(a, count(a) + 1) }
}
