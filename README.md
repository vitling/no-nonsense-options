# no-nonsense-options
![Maven Scala build](https://github.com/davw/no-nonsense-options/workflows/Maven%20Scala%20build/badge.svg)

no-nonsense-options is an Scala command-line options parser which uses [Shapeless](https://github.com/milessabin/shapeless) to
automatically derive predictable and useful command line interfaces for programs based on the data they need to run.

## At a glance

```scala

import io.github.davw.options.Cli

object MyApp extends App {
  case class Options(inputPath: String, outputPath: String, concurrency: Int = 4)
  val parsedOptions:Options = Cli.parseOrThrow[Options](args)
  println(parsedOptions)
}

```
This will create a command line interface which works like this

```
$ run_my_app --input-path ~/input_data --output-path ~/output_data --concurrency 15

Options(~/input_data,~/output_data,15)
```

Or if the user has passed invalid or missing options:

```
$ run_my_app

--input-path : required
--output-path : required
--concurrency : optional, defaults to 4
Exception in thread "main" io.github.davw.options.Cli$InvalidOptionsException: No value for  'inputPath' found in args

```

If you have a field which is of a non-trivial type, it is easy to provide a parse method

```scala
import io.github.davw.options.Cli.FieldParser
import java.io.File
implicit def dateFieldParser: FieldParser[File] = FieldParser(new File(_))
```

## Motivation

Most option parsing libraries and systems start from specifying the interface and then mapping that into data types that
can be used by the program. In observing many examples of the usage of this I saw a lot of redundancy and boilerplate
which requires no knowledge of the internal workings to produce. This can lead to errors in the mapping code, and
more moving parts to change if the CLI needs to be modified.

