package es.weso.shex.validator

import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import es.weso.rdf.operations.Comparisons._
import es.weso.shex._
import es.weso.utils.RegEx
import es.weso.shex.implicits.showShEx._
import cats._
import data._
import cats.implicits._
import cats.effect.IO
import es.weso.utils.IOUtils._
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.rdf.RDFBuilder

case class FacetChecker(schema: AbstractSchema, rdf: RDFReader, builder: RDFBuilder) 
  extends ShExChecker with ShowValidator {

  def checkFacets(attempt: Attempt, node: RDFNode)(facets: List[XsFacet]): CheckTyping =
    for {
      ts <- checkAll(facets.map(checkFacet(attempt, node)(_)))
      t  <- combineTypings(ts)
    } yield t

    
  private def checkFacet(attempt: Attempt, node: RDFNode)(facet: XsFacet): CheckTyping = for {
    s <- fromEitherIOS(facetChecker(node, facet))
    t <- addEvidence(attempt.nodeShape, s)
  } yield t

  
  def facetsChecker(node: RDFNode, facets: List[XsFacet]): EitherT[IO,String, String] = { 
    def cnv(pairs: (List[String], List[String])): Either[String,String] = {
      val (failed, passed) = pairs
      if (failed.isEmpty) {
        Right(s"$node passed facets: ${passed.map(_.show).mkString(",")}")
      } else {
        Left(s"""$node failed to pass facets ${facets.map(_.show).mkString(",")}
                |Failed facets: ${failed.mkString("\n")}
                |Passed facets: ${passed.mkString("\n")}
                |""".stripMargin)
      }
    }
    val ps: IO[(List[String], List[String])] = 
      partitionEitherIOS(facets.map(facetChecker(node, _))) 
    val v : IO[Either[String,String]] = ps.map(cnv(_))
    EitherT.apply(v)
  }

  private def facetChecker(node: RDFNode, facet: XsFacet): EitherT[IO, String, String] = {
    facet match {
      case Length(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l == n,
          s"${node.show} does not satisfy facet Length($n) with length $l",
          s"${node.show} satisfies Length($n) with length $l"
        )
      }
      case MinLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l >= n,
          s"${node.show} does not satisfy facet MinLength($n) with length $l",
          s"${node.show} satisfies MinLength($n) with length $l"
        )
      }
      case MaxLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l <= n,
          s"${node.show} does not satisfy facet MaxLength($n) with length $l",
          s"${node.show} satisfies MaxLength($n) with length $l"
        )
      }
      case Pattern(p, flags) => {
        val str = node.getLexicalForm
        // pprint.log(str, tag = "str")
        // pprint.log(p, tag = "p")
        RegEx(p, flags).matches(str) match {
          case Right(b) => {
            // pprint.log(b, tag = "b")
            checkCond(
              b,
              s"${node.show} does not match Pattern($p) with lexical form $str",
              s"${node.show} satisfies Pattern($p) with lexical form $str"
            )
          }
          case Left(msg) => EitherT.left(IO(msg))
        }
      }
      case MinInclusive(m) =>
        for {
          d <- EitherT.fromEither[IO](minInclusive(m, node))
          r <- checkCond(d,
            s"${node.show} does not match MinInclusive($m) with $node",
            s"${node.show} satisfies MinInclusive($m)"
          )
        } yield r
      case MinExclusive(m) =>
        for {
          d <- EitherT.fromEither[IO](minExclusive(m, node))
          r <- checkCond(
            d,
            s"${node.show} does not match MinExclusive($m) with $node",
            s"${node.show} satisfies MinExclusive($m)"
          )
        } yield r
      case MaxInclusive(m) =>
        for {
          d <- EitherT.fromEither[IO](maxInclusive(m, node))
          r <- checkCond(
            d,
            s"${node.show} does not match MaxInclusive($m) with $node",
            s"${node.show} satisfies MaxInclusive($m)"
          )
        } yield r
      case MaxExclusive(m) =>
        for {
          d <- EitherT.fromEither[IO](maxExclusive(m, node))
          r <- checkCond(
            d,
            s"${node.show} does not match MaxExclusive($m) with $node",
            s"${node.show} satisfies MaxExclusive($m)"
          )
        } yield r
      case FractionDigits(m) => {
        for {
          fd <- io2es(NodeInfo.fractionDigits(node, rdf))
          b <- checkCond(
                fd <= m,
                s"${node.show} does not match FractionDigits($m) with $node and fraction digits = $fd",
                s"${node.show} satisfies FractionDigits($m) with fraction digits = $fd"
              )
        } yield b
      }
      case TotalDigits(m) => {
        for {
          td <- io2es(NodeInfo.totalDigits(node, rdf))
          b <- checkCond(
                td <= m,
                s"${node.show} does not match TotalDigits($m) with $node and totalDigits = $td",
                s"${node.show} satisfies TotalDigits($m) with total digits = $td"
              )
        } yield b
      }
      case _ => EitherT.fromEither[IO](s"Not implemented checkFacet: $facet".asLeft[String])
    }
  }

  // TODO: I'd like to refactor the following code to avoid DRY...
  // Problem, how to do it in a compatible way with type safety
  private type Comparator = (NumericLiteral, RDFNode) => Either[String, Boolean]

  private def minInclusive: Comparator =
    (nl, node) =>
      for {
        nl2 <- numericValue(node)
      } yield lessThanOrEquals(nl, nl2)

  private def minExclusive: Comparator =
    (nl, node) =>
      for {
        nl2 <- numericValue(node)
      } yield lessThan(nl, nl2)

  private def maxInclusive: Comparator =
    (nl, node) =>
      for {
        nl2 <- numericValue(node)
      } yield lessThanOrEquals(nl2, nl)

  private def maxExclusive: Comparator =
    (nl, node) =>
      for {
        nl2 <- numericValue(node)
      } yield lessThan(nl2, nl)

  private def checkCond(cond: Boolean, msgFalse: => String, msgTrue: => String): EitherT[IO,String, String] = {
     if (cond) EitherT.fromEither[IO](msgTrue.asRight[String])
     else EitherT.fromEither[IO](msgFalse.asLeft[String])
  }

}
