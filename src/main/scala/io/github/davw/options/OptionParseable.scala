package io.github.davw.options

import java.time.LocalDate

import shapeless._
import shapeless.ops.record.Keys


sealed trait OptionParseable[T] {
  def describe(): String
}

class InvalidOptionsException(message: String) extends RuntimeException(message)

object OptionParseable extends LabelledTypeClassCompanion[OptionParseable] {

  def toArgForm(argName: String): String = "--" + argName

  type KVAFun[T] = (Map[String, String], Map[String, Option[_]]) => T

  case class FromKvArgs[T](fn: KVAFun[T], description: Map[String, String]) extends OptionParseable[T] {
    override def describe(): String = description.toString()
  }
  case class FromString[T](fn: String => T, description: String) extends OptionParseable[T] {
    override def describe(): String = description
  }

  case class FromQualifiedKvArgs[T](fn: String => KVAFun[T], description: Map[String, Map[String, String]]) extends OptionParseable[T] {
    override def describe(): String = description.toSeq.flatMap({ case (command, args) =>
      Seq(
        command
      ) ++ args.map({ case (arg, description) =>
        s" ${toArgForm(arg)} : $description"
      })
    }).reduce(_ + "\n" + _)
  }


  implicit val fsString: FromString[String] = FromString(identity, "string")
  implicit val fsDate: FromString[LocalDate] = FromString(s => LocalDate.parse(s), "date, in yyyy-MM-dd ISO form")
  implicit val fsInt: FromString[Int] = FromString(s => s.toInt, "int")

  object typeClass extends LabelledTypeClass[OptionParseable] {
    override def coproduct[L, R <: Coproduct](name: String, cl: => OptionParseable[L], cr: => OptionParseable[R]): OptionParseable[L :+: R] = (cl, cr) match {
      case (FromKvArgs(lFn, lDesc), FromQualifiedKvArgs(rFn, rDesc)) => FromQualifiedKvArgs(qualifier => (args, defs) => {
        if (qualifier==name) Inl(lFn(args,defs)) else Inr(rFn(qualifier)(args,defs))
      }, rDesc + (name -> lDesc))
    }

    override def emptyCoproduct: OptionParseable[CNil] = FromQualifiedKvArgs(qualifier => (args, defs) => {
      throw new InvalidOptionsException(s"Unknown command $qualifier")
    }, Map())

    override def product[H, T <: HList](name: String, ch: OptionParseable[H], ct: OptionParseable[T]): OptionParseable[H :: T] = (ch, ct) match {
      case (FromString(hFn, fieldDescription), FromKvArgs(tFn, tailDescription)) => FromKvArgs((argMap, defaultMap) => {
        if (argMap.contains(name)) {
          hFn(argMap(name)) :: tFn(argMap - name, defaultMap)
        } else if (defaultMap(name).isDefined) {
          defaultMap(name).get.asInstanceOf[H] :: tFn(argMap, defaultMap - name)
        } else {
          throw new InvalidOptionsException(s"Expected value for field $name to be provided with ${toArgForm(name)}, but no such value was found")
        }
      }, tailDescription + (name -> fieldDescription))
      case _ => throw new IllegalArgumentException
    }

    override def emptyProduct: OptionParseable[HNil] = FromKvArgs((hopefullyEmptyMap,_) => {
      if (hopefullyEmptyMap.isEmpty) HNil else throw new InvalidOptionsException(s"Additional arguments found that could not be parsed " + hopefullyEmptyMap.keys.map(toArgForm).reduce(_ + ", " + _))
    }, Map())

    override def project[F, G](instance: => OptionParseable[G], to: F => G, from: G => F): OptionParseable[F] = instance match {
      case FromString(gFn, desc) => FromString(str => from(gFn(str)), desc)
      case FromKvArgs(gFn, desc) => FromKvArgs((kv, d) => from(gFn(kv, d)), desc)
      case FromQualifiedKvArgs(gFn, desc) => FromQualifiedKvArgs((q) => (args, defs) => from(gFn(q)(args, defs)), desc)
    }
  }
}

object CliOld {
  import OptionParseable._

  def argsToMap(args: Seq[String]): Map[String, String] = {
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

  def run[T <: Runnable : OptionParseable](args: Seq[String]): Unit = {
    println(OptionParseable[T].describe())
  }
  def parse[T : OptionParseable: DefaultCapable](args: Seq[String]): T = OptionParseable[T] match {
    case FromString(fn, desc) => args match {
      case Seq(arg) => fn(arg)
      case Seq() => throw new InvalidOptionsException(s"Expected argument of type $desc, found none")
      case _ => throw new InvalidOptionsException(s"Expected single argument of type $desc, found multiple")
    }
    case FromKvArgs(fn, desc) => {
      val argMap = argsToMap(args)
      fn(argMap, defaults[T])
    }
    case FromQualifiedKvArgs(fn, desc) => {
      val command = args.head
      val argMap = argsToMap(args.tail)
      fn(command)(argMap, defaults[T])
    }
  }

  type DefaultMap = Map[String, Option[_]]

  trait DefaultCapable[T] {
    def getUntypedDefaults(): DefaultMap
  }
  object DefaultCapable {
    implicit def basicDefault[T]: DefaultCapable[T] = () => Map()
  }


  implicit def defFromGeneric[T, DType <: HList, GType <: HList, KType <: HList]
    (implicit de: Default.Aux[T, DType],
     gen: LabelledGeneric.Aux[T, GType],
     keys: Keys.Aux[GType, KType]): DefaultCapable[T] = () => {
    def toUntypedSeq(l : HList): Seq[Any] = l match {
      case HNil => Nil
      case head :: tail => Seq(head) ++ toUntypedSeq(tail)
    }
    (toUntypedSeq(keys.apply()).map(_.asInstanceOf[Symbol].name) zip toUntypedSeq(de.apply()).map(x => x.asInstanceOf[Option[_]])).toMap
  }

  def defaults[T : DefaultCapable]: DefaultMap = implicitly[DefaultCapable[T]].getUntypedDefaults()
}

object Main extends App {
  import OptionParseable._
  import Cli._

  case class MyApp(firstArg: String = "hello", secondArg: Int=14)
}

