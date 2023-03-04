package exercises02

object Counter {
  private val regexCountWords        = "[\\s.,!?:\\n\\t\\r()]"
  private val regexCountEnglishWords = "[А-Яа-я\\s.,!?:\\n\\t\\r()]"
  private val regexCountNumbers      = "[А-Яа-яA-Za-z\\s!?:\\-\\n\\t\\r()]"

  def veryNiceFunctionNmae(text: String, regex: String): Map[String, Int] =
    regex
      .split(text)
      .filter(_.nonEmpty)
      .groupMapReduce(_.toLowerCase())(_ => 1)(_ + _)

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countWords(text: String): Map[String, Int] = veryNiceFunctionNmae(text, regexCountWords)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  def countEnglishWords(text: String): Map[String, Int] = veryNiceFunctionNmae(text, regexCountEnglishWords)

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  def countNumbers(text: String): Map[String, Int] = veryNiceFunctionNmae(text, regexCountNumbers).filter(_._1 != ",")
}
