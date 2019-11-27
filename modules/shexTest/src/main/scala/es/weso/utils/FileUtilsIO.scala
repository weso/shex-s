package es.weso.utils
import cats.data.EitherT
import cats.effect._

object FileUtilsIO {

 def getContentsIO(path: String): EitherT[IO, String, String] = 
   EitherT (IO { FileUtils.getContents(path).map(_.toString) })
}