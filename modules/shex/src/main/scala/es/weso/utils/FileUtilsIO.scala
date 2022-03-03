package es.weso.utils
// import fs2._
// import cats.implicits._
// import java.nio.file.Paths
// import cats.effect.{Blocker, IO}

// import scala.concurrent.ExecutionContext

object FileUtilsIO {
  /*  def getContents(fileName: String): IO[CharSequence] = {
    val path = Paths.get(fileName)
    implicit val cs = IO.contextShift(ExecutionContext.global)
    val decoder: Pipe[IO,Byte,String] = fs2.text.utf8Decode
    Stream.resource(Blocker[IO]).flatMap(blocker =>
      fs2.io.file.readAll[IO](path, blocker,4096).through(decoder)
    ).compile.string
  } */

}
