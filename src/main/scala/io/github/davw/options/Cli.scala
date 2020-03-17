package io.github.davw.options

import shapeless._
import shapeless.ops.record.Keys

import scala.util.{Failure, Success, Try}
object Cli {

  type ParseError = String
  type ParseErrors = Seq[ParseError]

  /** Represents how a type OutputType can be parsed from a set of FieldNames and DefaultValues */
  trait KVParser[FieldNames, OutputType, DefaultValues] {
    def parse(kvArgs: Map[String, String], defaults: DefaultValues): Either[ParseErrors, OutputType]

    // A description of each of the fields in the OutputType. This is used to produce the "usage" string
    def fieldDescription(defaults: DefaultValues): Seq[String]
  }

  /** Represents how a single field (argument) can be parsed from a string parameter */
  trait FieldParser[T] {
    def fromString(s: String): Either[ParseError, T]
  }

  /** Create a FieldParser from a function that might throw an exception, catching it and transforming it to a Left(ParseError) */
  def fieldParser[T](possiblyThrowsException: String => T): FieldParser[T] = s => Try(possiblyThrowsException(s)) match {
    case Success(v) => Right(v)
    case Failure(f) => Left(f.toString)
  }

  /** Create a FieldParser from a function that follows the Haskell-style Either error pattern */
  def eitherFieldParser[T](convert: String => Either[ParseError, T]): FieldParser[T] = s => convert(s)

  // A default set of field parsers to deal with trivial cases
  implicit def fieldParserString: FieldParser[String] = fieldParser(identity)
  implicit def fieldParserInt: FieldParser[Int] = fieldParser(_.toInt)
  implicit def fieldParserLong: FieldParser[Long] = fieldParser(_.toLong)

  // We define implementations for HList forms of KVParser inductively. Here's the zero-case

  implicit def hNilParser:KVParser[HNil, HNil, HNil] = new KVParser[HNil, HNil, HNil] {
    override def parse(kvArgs: Map[String, String], defaults: HNil): Either[ParseErrors, HNil] =
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
    private def contextualise(error: ParseError): ParseError =
      s"A parse error occurred for field '${fieldNameWitness.value.name}': $error"

    override def parse(kvArgs: Map[String, String], defaults: DH :: DT): Either[ParseErrors, VH :: VT] = {
      // There's probably a clearer way to express this decision tree. I feel like there shouldn't be so many possibilities
      (kvArgs.get(fieldNameWitness.value.name), defaults.head: Option[VH]) match {
        case (Some(stringValue), _) => (fieldParser.fromString(stringValue), tailParser.parse(kvArgs - fieldNameWitness.value.name, defaults.tail)) match {
          case (Right(head), Right(tail)) => Right(head :: tail)
          case (Right(_), Left(tailError)) => Left(tailError)
          case (Left(error), Left(tailError)) => Left(Seq(contextualise(error)) ++ tailError)
          case (Left(error), Right(_)) => Left(Seq(contextualise(error)))
        }
        case (_, Some(defaultValue)) => tailParser.parse(kvArgs - fieldNameWitness.value.name, defaults.tail) match {
          case Right(success) => Right(defaultValue :: success)
          case Left(errors) => Left(errors)
        }
        case (_, _) => Left(Seq("No value for  '" + fieldNameWitness.value.name + "' found in args"))
      }
    }

    /** Append a description for the current field onto the list of descriptions for the tail */
    override def fieldDescription(defaults: DH :: DT): Seq[String] = {
      val defaultString = defaults.head.map(v => s"optional, defaults to $v").getOrElse("required")
      Seq(s"${fieldNameWitness.value.name} : $defaultString") ++ tailParser.fieldDescription(defaults.tail)
    }
  }

  /** Trait representing the high-level operations to interacting with the Cli API */
  trait ArgParser[T] {
    def fromCli(args: IterableOnce[String]): Either[ParseErrors, T]
    def usage(): String
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
    override def fromCli(args: IterableOnce[String]): Either[ParseErrors, CC] = {
      kvParser.parse(argsToMap(args), defaults.apply()) map gen.from
    }

    override def usage(): String = {
      kvParser.fieldDescription(defaults.apply()).mkString("\n")
    }
  }

  /** Exception class that wraps option parsing errors when throwing API is used */
  class InvalidOptionsException(str: String) extends IllegalArgumentException(str)

  /** Conversion of a string of args,
   *  eg. "--input this --output that" to a key-value map,
   *  eg. Map("input" -> "this", "output" -> "that")
   *  TODO: This is probably not as safe as it could be
   *  */
  private def argsToMap(args: IterableOnce[String]): Map[String, String] = {
    var map: Map[String, String] = Map()
    var key: String = null;
    for (arg <- args) {
      if (arg.startsWith("--")) {
        key = arg.substring(2)
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

  def parse[T : ArgParser](args: IterableOnce[String]): T = implicitly[ArgParser[T]].fromCli(args) match {
    case Right(v) => v
    case Left(err) => {
      println(implicitly[ArgParser[T]].usage())
      throw new InvalidOptionsException(err.mkString("\n"))
    }
  }
}
