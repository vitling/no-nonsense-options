package io.github.davw.options

import scala.util.{Failure, Success, Try}

/** Represents how a single field (argument) can be parsed from a string parameter */
trait FieldParser[T] {
  def fromString(s: String): Either[ParseError, T]
}

object FieldParser {

  /** Create a FieldParser from a function that might throw an exception, catching it and transforming it to a Left(ParseError) */
  def apply[T](possiblyThrowsException: String => T): FieldParser[T] = s => Try(possiblyThrowsException(s)) match {
    case Success(v) => Right(v)
    case Failure(f) => Left(ParseError(f.toString))
  }

  /** Create a FieldParser from a function that follows the Haskell-style Either error pattern */
  def either[T](convert: String => Either[ParseError, T]): FieldParser[T] = s => convert(s)
}

/** A default set of field parsers to deal with trivial cases */
trait SimpleFieldParsers {
  implicit def fieldParserString: FieldParser[String] = FieldParser(identity)
  implicit def fieldParserInt: FieldParser[Int] = FieldParser(_.toInt)
  implicit def fieldParserLong: FieldParser[Long] = FieldParser(_.toLong)
}

object UsefulFieldParsers {
  implicit def fieldParserOption[T : FieldParser]: FieldParser[Option[T]] = FieldParser.either(value => implicitly[FieldParser[T]].fromString(value).map(v => Some(v)))
}