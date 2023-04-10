package exercises07.ex02

import exercises07.data.NonEmptyList
import exercises07.ex02.Domain._
import exercises07.ex02.Errors.{InvalidAddressBookId, InvalidPersonId, InvalidPhone, MissingPersonName, ParsingError}
import exercises07.typeclasses._

object Exercise02 {
  type TransformationSupport[F[_]] = ApplicativeError[F, NonEmptyList[ParsingError]]

  private implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {
    def require[F[_]](err: => ParsingError)(implicit ts: TransformationSupport[F]): F[A] =
      opt match {
        case Some(value) => ts.pure(value)
        case None        => ts.raiseError(NonEmptyList.of(err))
      }
  }

  import TupleSyntax._
  import TransformerSyntax._
  import exercises07.ex01.Exercise01.Syntax._
  import exercises07.ex01.Exercise01.Instances._

  implicit def personTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawPerson, Person] =
    x =>
      (
        x.id.toLongOption.require(InvalidPersonId(x.id)),
        x.name.require(MissingPersonName),
        Phone.parse(x.phone).require(InvalidPhone(x.phone))
      ).mapN(Person)

  implicit def addressBookTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawAddressBook, AddressBook] =
    x =>
      (
        x.id.toLongOption.require(InvalidAddressBookId(x.id)),
        x.persons.traverse(_.transformF)
      ).mapN(AddressBook)
}
