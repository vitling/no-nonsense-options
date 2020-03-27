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

import io.github.davw.options.Cli.InvalidOptionsException
import shapeless.labelled.FieldType
import shapeless.{:+:, CNil, Coproduct, Default, Generic, HList, Inl, Inr, LabelledGeneric, Witness, labelled}
import shapeless.ops.record.Keys

/** Typeclass representing how a value of type T can be parsed from an sequence of command line arguments */
trait ArgParser[T] {
  def fromCli(args: Iterable[String]): Either[Seq[ParseError], T]
  def usage(): String
}

object ArgParser {
  def apply[T : ArgParser]: ArgParser[T] = implicitly[ArgParser[T]]
}

trait DervivedArgParsers {

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
    override def fromCli(args: Iterable[String]): Either[Seq[ParseError], CC] = {
      kvParser.parse(argsToMap(args), defaults.apply()) map gen.from
    }

    override def usage(): String = {
      kvParser.fieldDescription(defaults.apply()).mkString("\n")
    }
  }


  def transform[T, U](argParser: ArgParser[T], fn: T => U): ArgParser[U] = new ArgParser[U] {
    override def fromCli(args: Iterable[String]): Either[Seq[ParseError], U] = argParser.fromCli(args).map(fn)
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
    override def fromCli(args: Iterable[String]): Either[Seq[ParseError], CCF] = {
      args.headOption match {
        case Some(command) => cpParser.parser(command) match {
          case Some(parser) => parser.fromCli(args.tail).map(labelledGeneric.from)
          case None => Left(Seq(ParseError("The command specified does not match any known types that could be parsed into")))
        }
        case None => Left(Seq(ParseError("No command specified. The first arg to parse a sealed case class family must declare what concrete type to parse into")))
      }
    }
    override def usage(): String = "USAGE: app [command] [options], where [command] is one of:\n" + cpParser.options.map(command => {
      command + "\n" + cpParser.parser(command).get.usage() + "\n"
    }).mkString("\n")
  }

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
}