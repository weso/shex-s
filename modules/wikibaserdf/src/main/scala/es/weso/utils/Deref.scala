package es.weso.utils
import org.http4s._
import org.http4s.client._
import org.http4s.client.middleware._
// import org.http4s.dsl._
// import org.http4s.implicits._
import cats.effect._
import org.http4s.headers._
import scala.concurrent.ExecutionContext.global
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena._

import java.net._
// import java.net.http._
// import java.net.http.HttpResponse.BodyHandlers
// import java.net.http.HttpClient.Redirect
import java.time.Duration
import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.collection.JavaConverters._

object Deref {

//  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  def withRedirect[F[_]: Concurrent](c: Client[F]): Client[F] = FollowRedirect(10, _ => true)(c)

  def derefIRI(iri: Uri, client: Client[IO]): IO[String] = {
    lazy val `text/turtle` = new MediaType("text", "turtle")
    val redirectClient     = withRedirect(client)
    val req: Request[IO]   = Request(method = Method.GET, uri = iri).withHeaders(`Accept`(`text/turtle`))
    // val v: F[String] = redirectClient.expect[String](req)
    redirectClient.expect[String](req)
  }

  def iri2uri(iri: IRI): IO[Uri] = Uri
    .fromString(iri.str)
    .fold(e => IO.raiseError(new RuntimeException(s"Error converting $iri to Uri: $e")), IO.pure(_))

  def derefRDF(iri: IRI, client: Client[IO]): IO[Resource[IO, RDFAsJenaModel]] = for {
    uri <- iri2uri(iri)
    str <- derefIRI(uri, client)
    rdf <- RDFAsJenaModel.fromString(str, "TURTLE")
  } yield rdf

  def derefRDFJava(iri: IRI): IO[Resource[IO, RDFAsJenaModel]] = for {
    str <- IO {
      // This code is commented because it depends on Java 1.11
      /* val client = HttpClient.newBuilder().followRedirects(Redirect.ALWAYS).build()
      val request: HttpRequest = HttpRequest.newBuilder()
      .uri(iri.uri)
      .timeout(Duration.ofMinutes(4))
      .header("Accept", "text/turtle").GET.build()
      val response = client.send(request, BodyHandlers.ofString)
      response.body() */
      // Java 1.8 code
      val url: URL                = iri.uri.toURL
      val conn: HttpURLConnection = url.openConnection().asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setRequestProperty("Accept", "text/turtle")
      conn.setInstanceFollowRedirects(true)
      // I think redirects still are required to be done manually. See: https://mkyong.com/java/java-httpurlconnection-follow-redirect-example/
      conn.setReadTimeout(5000)
      val in  = new BufferedReader(new InputStreamReader(conn.getInputStream()))
      val str = in.lines().iterator.asScala.mkString
      conn.connect()
      str
    }
    rdf <- RDFAsJenaModel.fromString(str, "TURTLE")
  } yield rdf

}
