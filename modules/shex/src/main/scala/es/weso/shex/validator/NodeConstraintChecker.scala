package es.weso.shex.validator

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex._
import cats.effect.IO


case class NodeConstraintChecker(schema: Schema, rdf: RDFReader)
  extends ShowValidator(schema) with LazyLogging {

  def nodeConstraintChecker(value: RDFNode, nk: NodeConstraint
                           ): EitherT[IO,String, String] = {
   val rs = List(
      optCheck(nk.nodeKind, checkNodeKind(value)),
      optCheck(nk.values, checkValues(value)),
      optCheck(nk.datatype, checkDatatype(value)),
      checkXsFacets(value)(nk.xsFacets)
    ).sequence.map(_.mkString)
   // println(s"Result of nodeConstraintChecker: $rs")
   rs
  }

  private def checkNodeKind(node: RDFNode)(nk: NodeKind): EitherT[IO,String, String] =
   nk match {
        case IRIKind =>
          checkCond(node.isIRI, s"${node.show} is not an IRI", s"${node.show} is an IRI")
        case BNodeKind =>
          checkCond(node.isBNode, s"${node.show} is not a BlankNode", s"${node.show} is a BlankNode")
        case NonLiteralKind =>
          checkCond(!node.isLiteral, s"${node.show} is a literal but should be a NonLiteral",
            s"${node.show} is NonLiteral")
        case LiteralKind =>
          checkCond(node.isLiteral,s"${node.show} is not an Literal", s"${node.show} is a Literal")
  }

/*  private def checkCond(cond: Boolean, msgTrue: String, msgFalse: String): EitherT[IO,String, String] =
    if (cond) msgTrue.asRight[String]
    else msgFalse.asLeft[String] */

  private def checkValues(node: RDFNode)(values: List[ValueSetValue]): EitherT[IO,String, String] = {
    val checker = ValueChecker(schema)
    checkSome(values.map(v => checker.valueChecker(node,v)),
      s"Node doesn't belong to ${values.mkString(",")}"
    )
  }

  private def checkDatatype(node: RDFNode)(datatype: IRI): EitherT[IO,String, String] = for {
    b <- rdf.checkDatatype(node,datatype).recoverWith { 
       case e: Exception => IO(Left(s"Error trying to check if $node has datatype $datatype: ${e.getMessage}"))
    }
  } yield if (b) {
    Right(s"${node.show} has datatype ${datatype.show}")
  } else {
    Left(s"${node.show} doesn't have datatype ${datatype.show}")
  }


  private def checkXsFacets(node: RDFNode)(facets: List[XsFacet]): EitherT[IO,String, String] =
   if (facets.isEmpty) IO(Right(""))
   else {
    val r = FacetChecker(schema,rdf).facetsChecker(node,facets)
    // println(s"Result of facets checker: $r")
    r
  }

  private def optCheck[A, B](c: Option[A], check: A => IO[Either[String,String]]): EitherT[IO,String,String] =
    c.fold(IO("".asRight[String]))(check(_))

  def checkSome[A](cs: List[Either[String,String]], errorIfNone: String): Either[String,String] = {
    lazy val z: Either[String,String] = Left(errorIfNone)
    def comb(c1: Either[String,String], c2: Either[String,String]): Either[String,String] =
      c1 orElse c2
    cs.foldRight(z)(comb)
  }

}