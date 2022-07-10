package es.weso.wbmodel
import cats._
import cats.implicits._
import fs2._
import fs2.{Stream, text}
import fs2.io._
import fs2.compression._
import java.nio.file.Path
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import cats.effect._
import java.io._
import fs2.io.file.Files
import java.nio.file.{Files => JavaFiles, Paths}
import es.weso.shex
import java.nio.file.StandardOpenOption._
import org.wikidata.wdtk.datamodel.interfaces.PropertyDocument
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.JsonDeserializer
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import es.weso.wshex._
import es.weso.wbmodel._

case class DumpReaderError(msg: String) extends RuntimeException(msg)

sealed abstract class ParsedLine
case object OpenBracket extends ParsedLine
case object CloseBracket extends ParsedLine
case class ParsedEntity(entity: EntityDoc) extends ParsedLine
case class Error(str: String) extends ParsedLine
case object EndStream extends ParsedLine

/** Dump processor based on fs2
  */
object DumpReader {

//  val ChunkSize: Int = 4096
  private lazy val logger = LoggerFactory.getLogger(this.getClass().getCanonicalName());

  /** Process all entities in a file applying to each entity the function `withEntity` and storing the contents in `os`
    *
    * @param path
    * @param os
    * @param withEntity
    * @param opts
    * @return an IO action
    */
  def read[A: Monoid](
      is: InputStream,
      withEntity: EntityDoc => IO[A],
      opts: DumpReaderOptions = DumpReaderOptions.default
  ): IO[A] =
    readInputStream(is.pure[IO], opts.chunkSize)
      .through(when(opts.decompressInput, decompress))
      .through(text.utf8.decode)
      .through(text.lines)
      .zipWithIndex
      .parEvalMap(opts.maxConcurrent)(processLine(withEntity, opts))
      .foldMonoid
      .compile
      .foldMonoid

  def processLine[A: Monoid](withEntity: EntityDoc => IO[A], opts: DumpReaderOptions)(
      value: (String, Long)
  ): IO[A] = {
    val (line, index) = value
    for {
      parsedLine <- parseLine(line, opts)
      result <- processParsedLine(withEntity, parsedLine, index)
    } yield result
  }

  private def decompress: Pipe[IO, Byte, Byte] = s =>
    s.through(Compression[IO].gunzip()).flatMap(_.content)

  private def compress: Pipe[IO, Byte, Byte] = s => s.through(Compression[IO].gzip())

  private def when[A](
      cond: Boolean,
      action: => Stream[IO, A] => Stream[IO, A]
  ): Pipe[IO, A, A] =
    s =>
      if (cond) s.through(action)
      else s

  private def processParsedLine[A: Monoid](
      withEntity: EntityDoc => IO[A],
      parsedLine: ParsedLine,
      lineNumber: Long
  ): IO[A] =
    val empty = Monoid[A].empty
    parsedLine match {
      case OpenBracket     => empty.pure[IO] //  "[\n".pure[IO]
      case CloseBracket    => empty.pure[IO] // "]\n".pure[IO]
      case ParsedEntity(e) => withEntity(e)
      case Error(e) =>
        IO(println(s"Error at line $lineNumber: $e")) >> empty.pure[IO]
      case EndStream => empty.pure[IO]
    }

  private def parseLine(line: String, opts: DumpReaderOptions): IO[ParsedLine] =
    (line.trim match {
      case "[" => OpenBracket.pure[IO]
      case "]" => CloseBracket.pure[IO]
      case str =>
        EntityDoc.fromJsonStr(str, opts.jsonDeserializer).map(e => ParsedEntity(e))
    }).handleErrorWith(e =>
      e match {
        case e: MismatchedInputException => EndStream.pure[IO]
        case _                           => Error(e.getMessage()).pure[IO]
      }
    )

}
