package io.github.davw.options


import io.github.davw.options.OptionParseable.{FromKvArgs, FromQualifiedKvArgs, FromString}
import shapeless._
import shapeless.ops.hlist.ToList
import shapeless.ops.record.{Keys, Values}
import syntax.singleton._
import record._
object Cli {

  type ParseErrors = Seq[String]

  trait KVParser[FieldNames, OutputType, DefaultValues] {
    def parse(kvArgs: Map[String, String], defaults: DefaultValues): OutputType
  }

  trait FieldParser[T] {
    def fromString(s: String): T
  }

  implicit def fieldParserString: FieldParser[String] = g => g

  implicit def hNilParser:KVParser[HNil, HNil, HNil] = new KVParser[HNil, HNil, HNil] {
    override def parse(kvArgs: Map[String, String], defaults: HNil): HNil = HNil
  }
  implicit def hConsParser[
    FNH <: Symbol,
    FNT <: HList,
    VH,
    VT <: HList,
    DH <: Option[VH],
    DT <: HList](implicit fieldParser: FieldParser[VH], tailParser: KVParser[FNT, VT, DT], kw: Witness.Aux[FNH]): KVParser[FNH :: FNT, VH::VT, DH::DT] = new KVParser[FNH :: FNT, VH ::VT, DH::DT] {
    override def parse(kvArgs: Map[String, String], defaults: DH :: DT): VH :: VT = {
      ((kvArgs.get(kw.value.name), defaults.head: Option[VH]) match {
        case (Some(stringValue), _) => fieldParser.fromString(stringValue)
        case (_, Some(defaultValue)) => defaultValue
        case (_, _) => throw new IllegalArgumentException("No such field " + kw.value.name + " found in args " + kvArgs)
      }) :: tailParser.parse(kvArgs, defaults.tail)
    }
  }

  trait ArgParser[T] {
    def fromCli(args: IterableOnce[String]): Either[ParseErrors, T]
  }

  implicit def argParserForCaseClass[CC, R <: HList, K <: HList, V <: HList, D <: HList]
  (implicit gen: LabelledGeneric.Aux[CC, R], gen2: Generic.Aux[CC, V], keys: Keys.Aux[R, K], values: Values.Aux[R, V], defaults: Default.Aux[CC, D], zapper: KVParser[K, V, D]): ArgParser[CC] = new ArgParser[CC] {
    override def fromCli(args: IterableOnce[String]): Either[ParseErrors, CC] = {
      Right(gen2.from(zapper.parse(Utils.argsToMap(args.toSeq), defaults.apply())))
    }
  }

  object Utils {
    def argsToMap(args: IterableOnce[String]): Map[String, String] = {
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
  }

  def parse[T : ArgParser](args: IterableOnce[String]): T = implicitly[ArgParser[T]].fromCli(args).getOrElse(throw new IllegalArgumentException())

  private val rec = ("input" ->> "/dev/some") :: ("output" ->> "/dev/null") :: HNil
  case class Rec(input: String, output: String = "the dump")
  private val rec2 = Rec("/dev/some", "/dev/null")


  def main(args: Array[String]): Unit = {
    val out = parse[Rec](Seq("--input", "bark"))
    println(out)
  }

}
