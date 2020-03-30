# no-nonsense-options
[![Maven Scala build](https://github.com/davw/no-nonsense-options/workflows/Maven%20Scala%20build/badge.svg)](https://github.com/DavW/no-nonsense-options/actions?query=workflow%3A"Maven+Scala+build")

no-nonsense-options is an Scala command-line options parser which uses [Shapeless](https://github.com/milessabin/shapeless) to
automatically derive predictable and useful command line interfaces for programs based on the data they need to run.

## At a glance

```scala

import io.github.davw.options.Cli
import io.github.davw.options.Cli._

object MyApp extends App {
  case class Options(inputPath: String, outputPath: String, concurrency: Int = 4)
  val parsedOptions:Options = Cli.parseOrThrow[Options](args)
  println(parsedOptions)
}

```
This will create a command line interface which works like this

```
$ run_my_app --input-path /blah/input_data --output-path /blah/output_data --concurrency 15

Options(/blah/input_data,/blah/output_data,15)
```

Or if the user has passed invalid or missing options:

```
$ run_my_app

--input-path : required
--output-path : required
--concurrency : optional, defaults to 4
Exception in thread "main" io.github.davw.options.Cli$InvalidOptionsException: No value for  'inputPath' found in args

```

## Motivation

Most option parsing libraries and systems start from specifying the interface and then mapping that into data types that
can be used by the program. In observing many examples of the usage of this I saw a lot of redundancy and boilerplate
which requires no knowledge of the internal workings to produce. This can lead to errors in the mapping code, and
more moving parts to change if the CLI needs to be modified.

By inverting the problem, we encode all the conventions that people would naturally follow anyway in code, and the
list of possible options need only be specified once.

## Using in your project

Built for Scala 2.13. Cross-builds are not set up yet (PRs welcome for templating the pom or changing to sbt)

#### Maven
```xml
<dependency>
   <groupId>io.github.davw</groupId>
   <artifactId>no-nonsense-options</artifactId>
   <version>0.1.2</version>
</dependency>
```

#### sbt
*(note the single `%`, this is not cross-built)*
```scala
"io.github.davw" % "no-nonsense-options" % "0.1.2"
```

## API

The `Cli` object offers the following methods

```scala
def parse[T](args: Iterable[String]): Either[Seq[ParseError], T]
def parseOrThrow[T](args: Iterable[String]): T
def usage[T]: String
```
The `parseOrThrow` method will print usage and throw an exception if any `ParseError`s occurred: sometimes this is
totally sufficient for an application. For more control about how it deals with problems, use the `Either`-based 
interface:

### Example
```scala
import io.github.davw.options.Cli
import io.github.davw.options.Cli._

object ExampleCli {
  case class Input(a: Int, b: Int)

  def main(args: Array[String]): Unit = {
    Cli.parse[Input](args) match {
      case Left(errors) =>
        for (error <- errors) { println(error.message) }
        println(Cli.usage[Input])
        System.exit(1)
      case Right(input) => ??? /* Do something with input */
    }
  }
}
```

### Default values
Default values in your case class work in the way you'd expect. They are not required to be specified, but if they do
they will override the value specified as the default.

### Descriptive hints for usage text (0.2.0+)
If you need extra help text for an option to go in its usage text, just annotate that field with `@Hint("helpful text")`

### Multiple "commands" via sealed case class families

Primitive support is provided for parsing into range of different top-level options via representing those options as a
sealed case-class family
```scala
sealed trait Command
case class First(param: Long) extends Command
case class Second(somePath: String) extends Command
```
`Cli.parse[Command]` will accept arguments of either form, with the case class name as the first arg
```
$ run_my_app First --param 19482
$ run_my_app Second --some-path /path/to/a/thing
```
If you add something like a `run()` method to the `Command` trait, you can execute it directly on the returned `Command`
object, or else you can pattern-match against the result.

### Custom FieldParsers

If you have a field which is of a non-trivial type, it is easy to provide a parse method from String

Here, we create one for `File`. If an exception is thrown will get turned into a `ParseError`
```scala
implicit def fieldFieldParser: FieldParser[File] = FieldParser.create(new File(_))
```

Or we can use the `Either` interface directly via a single abstract method implementation.
This is often more practical when creating derived `FieldParser` implementations, as in the following example for
`Option`: 

```scala
implicit def fieldParserOption[T : FieldParser]: FieldParser[Option[T]] =
  value => FieldParser[T].fromString(value).map(v => Some(v))
```
*This is actually included in the `UsefulFieldParsers` object. It is useful because in conjunction with providing a
default value of `None` it can represent a truly optional argument.*


