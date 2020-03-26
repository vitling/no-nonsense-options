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

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.record.Keys

import scala.util.{Failure, Success, Try}

object Cli {

  type Problem = String
  type Problems = Seq[Problem]

  /** Represents how a type OutputType can be parsed from a set of FieldNames and DefaultValues */
  trait KVParser[FieldNames, OutputType, DefaultValues] {
    def parse(kvArgs: Map[String, String], defaults: DefaultValues): Either[Problems, OutputType]

    // A description of each of the fields in the OutputType. This is used to produce the "usage" string
    def fieldDescription(defaults: DefaultValues): Seq[String]
  }

  /** Represents how a single field (argument) can be parsed from a string parameter */
  trait FieldParser[T] {
    def fromString(s: String): Either[Problem, T]
  }

  object FieldParser {

    /** Create a FieldParser from a function that might throw an exception, catching it and transforming it to a Left(ParseError) */
    def apply[T](possiblyThrowsException: String => T): FieldParser[T] = s => Try(possiblyThrowsException(s)) match {
      case Success(v) => Right(v)
      case Failure(f) => Left(f.toString)
    }

    /** Create a FieldParser from a function that follows the Haskell-style Either error pattern */
    def either[T](convert: String => Either[Problem, T]): FieldParser[T] = s => convert(s)
  }

  // A default set of field parsers to deal with trivial cases
  implicit def fieldParserString: FieldParser[String] = FieldParser(identity)
  implicit def fieldParserInt: FieldParser[Int] = FieldParser(_.toInt)
  implicit def fieldParserLong: FieldParser[Long] = FieldParser(_.toLong)

  // We define implementations for HList forms of KVParser inductively. Here's the zero-case

  implicit def hNilParser:KVParser[HNil, HNil, HNil] = new KVParser[HNil, HNil, HNil] {
    override def parse(kvArgs: Map[String, String], defaults: HNil): Either[Problems, HNil] =
      if (kvArgs.nonEmpty)
        // If we have any args left here, they haven't been parsed from earlier fields, so we know that the user has
        // provided an argument that we don't know how to understand. This should be considered an error
        Left(Seq(s"Command line arguments ${kvArgs.keys} were provided but not understood"))
      else
        Right(HNil)
    // No argument usage information for the empty list
    override def fieldDescription(defaults: HNil): Seq[String] = Nil
  }

  implicit def hConsParser[
      FNH <: Symbol,     // The current field name
      FNT <: HList,      // The remaining field names
      VH,                // The current value type
      VT <: HList,       // The value type of the remaining list
      DH <: Option[VH],  // The default value for the current field. It is provably of either Some[VH] or None
      DT <: HList](implicit
                   fieldParser: FieldParser[VH],      // string parser for the current field
                   tailParser: KVParser[FNT, VT, DT], // link to the previous induction step
                   fieldNameWitness: Witness.Aux[FNH] // to extract field name value
      ): KVParser[FNH :: FNT, VH::VT, DH::DT] = new KVParser[FNH :: FNT, VH ::VT, DH::DT] {

    /** Add context to a field parsing error so we know which field was causing the error */
    private def contextualise(error: Problem): Problem =
      s"A parse error occurred for field '${fieldNameWitness.value.name}': $error"

    override def parse(kvArgs: Map[String, String], defaults: DH :: DT): Either[Problems, VH :: VT] = {
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
        case (_, _) => Left(Seq("No value for  '" + fieldNameWitness.value.name + "' found in args"))
      }
    }

    /** Append a description for the current field onto the list of descriptions for the tail */
    override def fieldDescription(defaults: DH :: DT): Seq[String] = {
      val defaultString = defaults.head.map(v => s"optional, defaults to $v").getOrElse("required")
      Seq(s"${fieldNameToArgName(fieldNameWitness.value.name)} : $defaultString") ++ tailParser.fieldDescription(defaults.tail)
    }
  }

  /** Trait representing the high-level operations to interacting with the Cli API */
  trait ArgParser[T] {
    def fromCli(args: Iterable[String]): Either[Problems, T]
    def usage(): String
  }

  object ArgParser {
    def apply[T : ArgParser]: ArgParser[T] = implicitly[ArgParser[T]]
  }

  /**
   * Here we derive an arg parser for an arbitrary case class, using the mappings between case classes and HLists
   * provided by the Generic and LabelledGeneric mechanisms available in Shapeless. It should be implicitly resolvable if:
   * - CC is a case class
   * - All fields in the case class have a FieldParser implementation available on the implicit list
   */
  implicit def argParserForCaseClass[
    CC,         // Case class we're deriving for
    R <: HList, // Record-typed translation of case-class type
    K <: HList, // Simple HList of field name symbols
    V <: HList, // Simple HList type for field values
    D <: HList] // Simple HList of default values provided in case class definition
  (implicit
   lGen: LabelledGeneric.Aux[CC, R], // Used only to extract field name key type
   gen: Generic.Aux[CC, V],          // Conversion from finished V record to case class
   keys: Keys.Aux[R, K],             // Used in conjunction with lGen to extract field name key type
   defaults: Default.Aux[CC, D],     // Extracts default values from case class definition
   kvParser: KVParser[K, V, D])      // Our inductively defined KVParser implementation from above
  : ArgParser[CC] = new ArgParser[CC] {
    override def fromCli(args: Iterable[String]): Either[Problems, CC] = {
      kvParser.parse(argsToMap(args), defaults.apply()) map gen.from
    }

    override def usage(): String = {
      kvParser.fieldDescription(defaults.apply()).mkString("\n")
    }
  }


  def transform[T, U](argParser: ArgParser[T], fn: T => U): ArgParser[U] = new ArgParser[U] {
    override def fromCli(args: Iterable[String]): Either[Problems, U] = argParser.fromCli(args).map(fn)
    override def usage(): String = argParser.usage()
  }

  /**
   * Represents a set of "command" types that could be parsed, based on a discriminating "command" name
   * We will use this to parsed sealed case class families, via shapeless' discriminated coproduct mapping
   * */
  trait DiscriminatedParser[A] {
    /** Retrieve a parser for the given command name, if available */
    def parser(command: String): Option[ArgParser[A]]

    /** List available commands */
    def options: Seq[String]
  }

  // In the next 2 definitions we inductively define a DiscriminatedParser implementation for arbitrary labelled
  // Coproducts (also known as discriminated unions).

  implicit def cNilDiscriminatedParser: DiscriminatedParser[CNil] = new DiscriminatedParser[CNil] {
    // If we have got this far then the command didn't match any known commands, so we propagate the "None" vaue back up the stack
    override def parser(command: String): Option[ArgParser[CNil]] = None
    override def options: Seq[String] = Nil
  }

  import labelled.field
  implicit def coproductSelector[
    KL <: Symbol,    // Name of the head command
    VL,              // Type of the head command
    CR <: Coproduct] // The tail of the coproduct
   (implicit keyWitness: Witness.Aux[KL], // To extract the value of the command name
    argParser: ArgParser[VL],             // A parser for the head command type
    tail: DiscriminatedParser[CR])        // If it's not a match, we defer to the Coproduct tail
  : DiscriminatedParser[FieldType[KL, VL] :+: CR] = new DiscriminatedParser[FieldType[KL, VL] :+: CR] {
      override def parser(command: String): Option[ArgParser[FieldType[KL, VL] :+: CR]] = {
        if (command == keyWitness.value.name)
          Some(transform(argParser, (x: VL) => Inl[FieldType[KL, VL], CR](field[KL](x))))
        else
          tail.parser(command).map(parser => transform(parser, (x: CR) => Inr[FieldType[KL, VL], CR](x)))
      }
      override def options: Seq[String] = Seq(keyWitness.value.name) ++ tail.options
    }

  /** Using the DiscriminatedParser implementation that can be derived from the isomorphic Coproduct to a sealed case
   *  class family, we can simply define an ArgParser implementation for the case class family, by taking the first
   *  argument to determine a "command" that selects which parser to use */
  implicit def genericSelector[CCF, CP](implicit labelledGeneric: LabelledGeneric.Aux[CCF, CP], cpParser: DiscriminatedParser[CP]): ArgParser[CCF] = new ArgParser[CCF] {
    override def fromCli(args: Iterable[String]): Either[Problems, CCF] = {
      args.headOption match {
        case Some(command) => cpParser.parser(command) match {
          case Some(parser) => parser.fromCli(args.tail).map(labelledGeneric.from)
          case None => Left(Seq("The command specified does not match any known types that could be parsed into"))
        }
        case None => Left(Seq("No command specified. The first arg to parse a sealed case class family must declare what concrete type to parse into"))
      }
    }
    override def usage(): String = "USAGE: app [command] [options], where [command] is one of:\n" + cpParser.options.map(command => {
      command + "\n" + cpParser.parser(command).get.usage() + "\n"
    }).mkString("\n")
  }

  /** Exception class that wraps option parsing errors when throwing API is used */
  class InvalidOptionsException(str: String) extends IllegalArgumentException(str)

  /** Conversion of a string of args,
   *  eg. "--input this --output that" to a key-value map,
   *  eg. Map("input" -> "this", "output" -> "that")
   *  TODO: This is probably not as safe as it could be
   *  */
  private def argsToMap(args: Iterable[String]): Map[String, String] = {
    var map: Map[String, String] = Map()
    var key: String = null;
    for (arg <- args) {
      if (arg.startsWith("--")) {
        key = arg
      } else {
        if (key == null) {
          throw new InvalidOptionsException(s"Found arg value '$arg', but no corresponding '--' option")
        }
        map = map + (key -> arg)
        key = null
      }
    }
    map
  }

  private[options] def fieldNameToArgName(fieldName: String): String = {
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

  def parse[T : ArgParser](args: Iterable[String]): Either[Problems, T] = ArgParser[T].fromCli(args)
  def usage[T : ArgParser]: String = ArgParser[T].usage();

  def parseOrThrow[T : ArgParser](args: Iterable[String]): T = ArgParser[T].fromCli(args) match {
    case Right(v) => v
    case Left(err) =>
      println(implicitly[ArgParser[T]].usage())
      throw new InvalidOptionsException(err.mkString("\n"))
  }
}
