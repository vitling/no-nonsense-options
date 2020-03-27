/**
 * Copyright 2020 David Whiting
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.davw.options

import scala.util.{Failure, Success, Try}

/** Represents how a single field (argument) can be parsed from a string parameter */
trait FieldParser[T] {
  def fromString(s: String): Either[ParseError, T]
}

object FieldParser {

  def apply[T : FieldParser]: FieldParser[T] = implicitly

  /** Create a FieldParser from a function that might throw an exception, catching it and transforming it to a Left(ParseError) */
  def create[T](possiblyThrowsException: String => T): FieldParser[T] = s => Try(possiblyThrowsException(s)) match {
    case Success(v) => Right(v)
    case Failure(f) => Left(ParseError(f.toString))
  }

  /** Create a FieldParser from a function that follows the Haskell-style Either error pattern */
    // TODO does this provide any value when we can specify a FieldParser using a single abstract method?
  def createEither[T](convert: String => Either[ParseError, T]): FieldParser[T] = s => convert(s)
}

/** A default set of field parsers to deal with trivial cases */
trait SimpleFieldParsers {
  implicit def fieldParserString: FieldParser[String] = FieldParser.create(identity)
  implicit def fieldParserInt: FieldParser[Int] = FieldParser.create(_.toInt)
  implicit def fieldParserLong: FieldParser[Long] = FieldParser.create(_.toLong)
}

object UsefulFieldParsers {
  implicit def fieldParserOption[T : FieldParser]: FieldParser[Option[T]] = value => FieldParser[T].fromString(value).map(v => Some(v))
}