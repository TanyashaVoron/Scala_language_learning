package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._

import scala.util.matching.Regex

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
      if passport.matches("\\d{4} \\d{6}")
    */
  private val passportPattern: Regex = "(\\d{4}) (\\d{6})".r
  private def parsePassport(passport: String): Option[Passport] = {
    passport match {
      case passportPattern(s, num) => Some(Passport(s.toLong, num.toLong))
      case _                       => None
    }
  }

  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      firstName  <- rawUser.firstName
      secondName <- rawUser.secondName
      passport_ = rawUser.passport.flatMap(parsePassport)
      if rawUser.passport.isEmpty || passport_.isDefined
      if !rawUser.banned
      id <- rawUser.id.toLongOption
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport_)

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] = {
    for {
      _  <- if (!rawUser.banned) Right(()) else Left(Banned)
      id <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      passportOpt <- Either.fromOption(
        if (rawUser.passport.isEmpty) Some(None) else rawUser.passport.flatMap(parsePassport).map(Some(_))
      )(InvalidPassport)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passportOpt)
  }
}
