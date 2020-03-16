package io.github.davw.options

import org.scalatest._

class CliSpec extends FlatSpec {
  import OptionParseable._
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

//  "A single value" should "be parsed from a single arg" in {
//    assert(Cli.parse[String](Seq("hello")) == "hello")
//    assert(Cli.parse[Int](Seq("24")) == 24)
//  }
//
//  it should "not be parsed with no args" in {
//    assertThrows[InvalidOptionsException] {
//      Cli.parse[String](Seq())
//    }
//  }
//
//  it should "not be parsed with multiple args" in {
//    assertThrows[InvalidOptionsException] {
//      Cli.parse[String](Seq("first", "second"))
//    }
//  }

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
    assert(exception.getMessage.contains("--throughput"))
  }

//  sealed trait Commands
//  case class CommandOne(input: String, output: String, mode: String) extends Commands
//  case class CommandTwo(input: String, throughput: String) extends Commands
//
//  "A sealed case class family" should "be parsed with a qualifying first argument" in {
//    assert(Cli.parse[Commands](Seq("CommandOne", "--input", "my_input", "--output", "my_output", "--mode", "mode_one")) == CommandOne("my_input", "my_output", "mode_one"))
//    assert(Cli.parse[Commands](Seq("CommandTwo", "--input", "my_input", "--throughput", "my_throughput")) == CommandTwo("my_input", "my_throughput"))
//  }

  case class CommandWithDefaults(input: String, output: String = "/dev/null")

  "A case class with a default value" should "be parsed with or without the argument with a default value being passed" in {
    assert(Cli.parse[CommandWithDefaults](Seq("--input", "/dev/random", "--output", "elsewhere")) == CommandWithDefaults("/dev/random", "elsewhere"))
    assert(Cli.parse[CommandWithDefaults](Seq("--input", "/dev/random")) == CommandWithDefaults("/dev/random"))
  }


}
