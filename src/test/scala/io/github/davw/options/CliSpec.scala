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

import io.github.davw.options
import jdk.jfr.Description
import org.scalatest._

class CliSpec extends FlatSpec {
  import Cli._

  case class EmptyArgs()

  "An empty case class" should "be parsed from empty args" in {
    assert(Cli.parseOrThrow[EmptyArgs](Seq()) == EmptyArgs())
  }

  it should "not be parsed from a single arg" in {
    assertThrows[InvalidOptionsException] {
      Cli.parseOrThrow[EmptyArgs](Seq("hello"))
    }
  }

  it should "not be parsed from multiple args" in {
    assertThrows[InvalidOptionsException] {
      Cli.parseOrThrow[EmptyArgs](Seq("hello", "goodbye"))
    }
  }

  case class SimpleApp(input: String, output: String)

  "A simple case class" should "be parsed from predictable arguments in any order" in {
    val expected = SimpleApp("my_input", "my_output")
    assert(Cli.parseOrThrow[SimpleApp](Seq("--input", "my_input", "--output", "my_output")) == expected)
    assert(Cli.parseOrThrow[SimpleApp](Seq("--output", "my_output", "--input", "my_input")) == expected)
  }

  it should "not be parsed with options missing" in {
    assertThrows[InvalidOptionsException] {
      Cli.parseOrThrow[SimpleApp](Seq("--input", "my_input"))
    }
  }

  it should "not be parsed with extra options provided" in {
    val exception = intercept[InvalidOptionsException] {
      Cli.parseOrThrow[SimpleApp](Seq("--input", "my_input", "--output", "my_output", "--throughput", "my_throughput"))
    }
    assert(exception.getMessage.contains("throughput"))
  }

  sealed trait Commands
  case class CommandOne(input: String, output: String, mode: String) extends Commands
  case class CommandTwo(input: String, throughput: String) extends Commands

  "A sealed case class family" should "be parsed with a qualifying first argument" in {
    assert(Cli.parseOrThrow[Commands](Seq("CommandOne", "--input", "my_input", "--output", "my_output", "--mode", "mode_one")) == CommandOne("my_input", "my_output", "mode_one"))
    assert(Cli.parseOrThrow[Commands](Seq("CommandTwo", "--input", "my_input", "--throughput", "my_throughput")) == CommandTwo("my_input", "my_throughput"))
  }

  case class CommandWithDefaults(input: String, output: String = "/dev/null")

  "A case class with a default value" should "be parsed with or without the argument with a default value being passed" in {
    assert(Cli.parseOrThrow[CommandWithDefaults](Seq("--input", "/dev/random", "--output", "elsewhere")) == CommandWithDefaults("/dev/random", "elsewhere"))
    assert(Cli.parseOrThrow[CommandWithDefaults](Seq("--input", "/dev/random")) == CommandWithDefaults("/dev/random"))
  }


  case class CommandWithOtherFieldTypes(age: Int)

  "A case class with non-string field types" should "be parsed with a correct value" in {
    assert(Cli.parseOrThrow[CommandWithOtherFieldTypes](Seq("--age" ,"21")) == CommandWithOtherFieldTypes(21))
  }

  it should "return a meaningful error when an arg is unparseable" in {
    val exception = intercept[InvalidOptionsException] {
      Cli.parseOrThrow[CommandWithOtherFieldTypes](Seq("--age", "twentyone"))
    }
    assert(exception.getMessage.contains("age"))
    assert(exception.getMessage.contains("NumberFormatException"))
  }


  case class MyCustomType(firstPart: String, secondPart: String) {
    override def toString: String = firstPart + ":" + secondPart
  }

  implicit val customTypeParser: FieldParser[MyCustomType] = FieldParser.createEither(_.split(":") match {
    case Array(firstPart, secondPart) => Right(MyCustomType(firstPart, secondPart))
    case _ => Left(ParseError("Expected string in the form a:b"))
  })

  case class CommandWithCustomFieldType(name: String, sections: MyCustomType)

  "A case class with a custom field type" should "be parsed by an implicitly-available custom parser" in {
    assert(Cli.parseOrThrow[CommandWithCustomFieldType](Seq("--name", "David", "--sections", "aaaa:bbbb")) == CommandWithCustomFieldType("David", MyCustomType("aaaa", "bbbb")))
  }

  it should "provide a useful error when the custom parser cannot parse the field type" in {
    val exception = intercept[InvalidOptionsException] {
      Cli.parseOrThrow[CommandWithCustomFieldType](Seq("--name", "David", "--sections", "aaaa:bbbb:cccc"))
    }
    assert(exception.getMessage.contains("Expected string in the form a:b"))
  }

  case class CommandWithMultiWordNames(inputPath: String, outputFilePattern: String)
  "A case class with multi-world field names" should "take options in the hyphenated form" in {
    assert(Cli.parseOrThrow[CommandWithMultiWordNames](Seq("--input-path", "/blah", "--output-file-pattern", "/blorg/%/blip")) == CommandWithMultiWordNames("/blah", "/blorg/%/blip"))
  }

  it should "use the hyphenated field names in the usage text" in {
    val usage = Cli.usage[CommandWithMultiWordNames]
    assert(usage contains "--input-path")
    assert(usage contains "--output-file-pattern")
  }

  import UsefulFieldParsers.fieldParserOption

  case class CommandWithOptionValues(name: String, greeting: Option[String] = None)

  "A command with values of type Option[T]" should "parse with or without the option being passed" in {
    assert(Cli.parseOrThrow[CommandWithOptionValues](Seq("--name", "David")) == CommandWithOptionValues("David", None))
    assert(Cli.parseOrThrow[CommandWithOptionValues](Seq("--name", "David", "--greeting", "Hi there!")) == CommandWithOptionValues("David", Some("Hi there!")))
  }

  case class DescribedOptions(@options.Hint("xyzzy") input: String, @options.Hint("abcde") output: String)

  "A case class with descriptive text annotated in the options" should "include the description in the usage text" in {
    val usage = Cli.usage[DescribedOptions]
    println(usage)
    assert(usage contains "xyzzy")
    assert(usage contains "abcde")
  }
}
