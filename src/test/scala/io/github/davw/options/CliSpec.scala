package io.github.davw.options

import org.scalatest._

class CliSpec extends FlatSpec {
  import Cli._

  case class EmptyArgs()

  "An empty case class" should "be parsed from empty args" in {
    assert(Cli.parse[EmptyArgs](Seq()) == EmptyArgs())
  }

  it should "not be parsed from a single arg" in {
    assertThrows[InvalidOptionsException] {
      Cli.parse[EmptyArgs](Seq("hello"))
    }
  }

  it should "not be parsed from multiple args" in {
    assertThrows[InvalidOptionsException] {
      Cli.parse[EmptyArgs](Seq("hello", "goodbye"))
    }
  }

  case class SimpleApp(input: String, output: String)

  "A simple case class" should "be parsed from predictable arguments in any order" in {
    val expected = SimpleApp("my_input", "my_output")
    assert(Cli.parse[SimpleApp](Seq("--input", "my_input", "--output", "my_output")) == expected)
    assert(Cli.parse[SimpleApp](Seq("--output", "my_output", "--input", "my_input")) == expected)
  }

  it should "not be parsed with options missing" in {
    assertThrows[InvalidOptionsException] {
      Cli.parse[SimpleApp](Seq("--input", "my_input"))
    }
  }

  it should "not be parsed with extra options provided" in {
    val exception = intercept[InvalidOptionsException] {
      Cli.parse[SimpleApp](Seq("--input", "my_input", "--output", "my_output", "--throughput", "my_throughput"))
    }
    assert(exception.getMessage.contains("throughput"))
  }

  sealed trait Commands
  case class CommandOne(input: String, output: String, mode: String) extends Commands
  case class CommandTwo(input: String, throughput: String) extends Commands

  "A sealed case class family" should "be parsed with a qualifying first argument" in {
    assert(Cli.parse[Commands](Seq("CommandOne", "--input", "my_input", "--output", "my_output", "--mode", "mode_one")) == CommandOne("my_input", "my_output", "mode_one"))
    assert(Cli.parse[Commands](Seq("CommandTwo", "--input", "my_input", "--throughput", "my_throughput")) == CommandTwo("my_input", "my_throughput"))
  }

  case class CommandWithDefaults(input: String, output: String = "/dev/null")

  "A case class with a default value" should "be parsed with or without the argument with a default value being passed" in {
    assert(Cli.parse[CommandWithDefaults](Seq("--input", "/dev/random", "--output", "elsewhere")) == CommandWithDefaults("/dev/random", "elsewhere"))
    assert(Cli.parse[CommandWithDefaults](Seq("--input", "/dev/random")) == CommandWithDefaults("/dev/random"))
  }


  case class CommandWithOtherFieldTypes(age: Int)

  "A case class with non-string field types" should "be parsed with a correct value" in {
    assert(Cli.parse[CommandWithOtherFieldTypes](Seq("--age" ,"21")) == CommandWithOtherFieldTypes(21))
  }

  it should "return a meaningful error when an arg is unparseable" in {
    val exception = intercept[InvalidOptionsException] {
      Cli.parse[CommandWithOtherFieldTypes](Seq("--age", "twentyone"))
    }
    assert(exception.getMessage.contains("age"))
    assert(exception.getMessage.contains("NumberFormatException"))

  }


}
