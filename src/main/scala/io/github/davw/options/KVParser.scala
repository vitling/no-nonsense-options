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

import shapeless.{::, HList, HNil, Witness}

/** Represents how a type OutputType with the given FieldNames can be parsed from a String->String K-V map and
 *  a set of default values */
trait KVParser[FieldNames, OutputType, DefaultValues, FieldDescriptions] {
  def parse(kvArgs: Map[String, String], defaults: DefaultValues): Either[Seq[ParseError], OutputType]

  /** A description of each of the fields in the OutputType. This is used to produce the "usage" string */
  def fieldDescription(defaults: DefaultValues, descriptions: FieldDescriptions): Seq[String]
}

trait DerivedKVParsers {
  // We define implementations for HList forms of KVParser inductively. Here's the zero-case
  implicit def hNilParser:KVParser[HNil, HNil, HNil, HNil] = new KVParser[HNil, HNil, HNil, HNil] {
    override def parse(kvArgs: Map[String, String], defaults: HNil): Either[Seq[ParseError], HNil] =
      if (kvArgs.nonEmpty)
      // If we have any args left here, they haven't been parsed from earlier fields, so we know that the user has
      // provided an argument that we don't know how to understand. This should be considered an error
      Left(Seq(ParseError(s"Command line arguments ${kvArgs.keys} were provided but not understood")))
      else
      Right(HNil)
    // No argument usage information for the empty list
    override def fieldDescription(defaults: HNil, descriptions: HNil): Seq[String] = Nil
  }

  implicit def hConsParser[
    FNH <: Symbol,     // The current field name
    FNT <: HList,      // The remaining field names
    VH,                // The current value type
    VT <: HList,       // The value type of the remaining list
    DH <: Option[VH],  // The default value for the current field. It is provably of either Some[VH] or None
    DT <: HList,
    DescH <: Option[Hint],
    DescT <: HList
  ](implicit
                 fieldParser: FieldParser[VH],      // string parser for the current field
                 tailParser: KVParser[FNT, VT, DT, DescT], // link to the previous induction step
                 fieldNameWitness: Witness.Aux[FNH] // to extract field name value
                ): KVParser[FNH :: FNT, VH::VT, DH::DT, DescH::DescT] = new KVParser[FNH :: FNT, VH ::VT, DH::DT, DescH::DescT] {

    /** Add context to a field parsing error so we know which field was causing the error */
    private def contextualise(error: ParseError): ParseError =
      ParseError(s"A parse error occurred for field '${fieldNameWitness.value.name}': ${error.message}")

    override def parse(kvArgs: Map[String, String], defaults: DH :: DT): Either[Seq[ParseError], VH :: VT] = {
      val argName = fieldNameToArgName(fieldNameWitness.value.name)
      // There's probably a clearer way to express this decision tree. I feel like there shouldn't be so many possibilities
      (kvArgs.get(argName), defaults.head: Option[VH]) match {
        case (Some(stringValue), _) => (fieldParser.fromString(stringValue), tailParser.parse(kvArgs - argName, defaults.tail)) match {
          case (Right(head), Right(tail)) => Right(head :: tail)
          case (Right(_), Left(tailError)) => Left(tailError)
          case (Left(error), Left(tailError)) => Left(Seq(contextualise(error)) ++ tailError)
          case (Left(error), Right(_)) => Left(Seq(contextualise(error)))
        }
        case (_, Some(defaultValue)) => tailParser.parse(kvArgs - argName, defaults.tail) match {
          case Right(success) => Right(defaultValue :: success)
          case Left(errors) => Left(errors)
        }
        case (_, _) => Left(Seq(ParseError("No value for  '" + fieldNameWitness.value.name + "' found in args")))
      }
    }

    /** Append a description for the current field onto the list of descriptions for the tail */
    override def fieldDescription(defaults: DH :: DT, descriptions: DescH :: DescT): Seq[String] = {
      val defaultString = defaults.head.map(v => s"optional, defaults to $v").getOrElse("required")
      val descriptionString = descriptions.head.map(" - " + _.text).getOrElse("")
      Seq(s"${fieldNameToArgName(fieldNameWitness.value.name)} : $defaultString $descriptionString") ++ tailParser.fieldDescription(defaults.tail, descriptions.tail)
    }
  }

  private def fieldNameToArgName(fieldName: String): String = {
    var words = Seq[String]()
    var partialWord = ""
    for (char <- fieldName) {
      if (partialWord.nonEmpty && partialWord.last.isLower && char.isUpper) {
        words = words :+ partialWord
        partialWord = ""
      }
      partialWord = partialWord + char
    }
    words = words :+ partialWord
    "--" + words.map(_.toLowerCase).mkString("-")
  }
}