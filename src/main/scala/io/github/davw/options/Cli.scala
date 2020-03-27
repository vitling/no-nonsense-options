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


object Cli extends DerivedKVParsers with DervivedArgParsers with SimpleFieldParsers {

  /** Exception class that wraps option parsing errors when throwing API is used */
  class InvalidOptionsException(str: String) extends IllegalArgumentException(str)

  def parse[T : ArgParser](args: Iterable[String]): Either[Seq[ParseError], T] = ArgParser[T].fromCli(args)
  def usage[T : ArgParser]: String = ArgParser[T].usage()

  def parseOrThrow[T : ArgParser](args: Iterable[String]): T = ArgParser[T].fromCli(args) match {
    case Right(v) => v
    case Left(err) =>
      println(implicitly[ArgParser[T]].usage())
      throw new InvalidOptionsException(err.map(_.message).mkString("\n"))
  }
}
