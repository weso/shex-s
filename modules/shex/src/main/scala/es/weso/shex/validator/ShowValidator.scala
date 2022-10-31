package es.weso.shex.validator
import es.weso.rdf.nodes._
import cats._
import implicits._
import es.weso.shex._
import es.weso.rdf.triples.RDFTriple
import es.weso.shex.implicits.showShEx._

trait ShowValidator {
  val schema: AbstractSchema

  implicit lazy val showIRI: Show[IRI] = new Show[IRI] {
    override def show(i: IRI): String =
      schema.qualify(i)
  }

  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    override def show(n: RDFNode): String =
      n match {
        case i: IRI     => i.show
        case l: Literal => l.getLexicalForm
        case b: BNode   => "_:" + b.getLexicalForm
      }
  }

  implicit lazy val showRDFTriple: Show[RDFTriple] = new Show[RDFTriple] {
    override def show(n: RDFTriple): String =
      s"<${n.subj.show},${n.pred.show},${n.obj.show}>"
  }

  implicit lazy val showShapeLabel: Show[ShapeLabel] = new Show[ShapeLabel] {
    override def show(lbl: ShapeLabel): String =
      lbl match {
        case l: IRILabel   => Show[RDFNode].show(l.iri)
        case l: BNodeLabel => Show[RDFNode].show(l.bnode)
        case Start         => "Start"
      }
  }

  implicit lazy val showPath: Show[Path] = new Show[Path] {
    override def show(p: Path): String =
      p match {
        case Direct(iri)  => schema.qualify(iri)
        case Inverse(iri) => "^" + schema.qualify(iri)
      }
  }

  implicit lazy val showAttempt: Show[Attempt] = new Show[Attempt] {
    override def show(a: Attempt): String = {
      val showPath: String = a.path match {
        case None    => ""
        case Some(p) => ", path: " + p.show
      }
      s"Attempt: node: ${a.node.show}, shape: ${a.shape.show}${showPath}"
    }
  }

  
  def sh(lbls: Set[ShapeLabel]): String =
    if (lbls.isEmpty) "{}"
    else lbls.map(lbl => schema.prefixMap.qualify(lbl.toRDFNode)).mkString(",")

  def showSE(s: ShapeExpr): String =
    s.id.map(lbl => schema.prefixMap.qualify(lbl.toRDFNode))
      .getOrElse(s match {
        case sa: ShapeAnd       => s"AND(${sa.shapeExprs.map(showSE(_).mkString(","))})"
        case sa: ShapeOr        => s"OR(@@${sa.shapeExprs.map(showSE(_).mkString(","))})"
        case sn: ShapeNot       => s"NOT(${showSE(sn.shapeExpr)})"
        case sd: ShapeDecl      => s"Decl(${showSE(sd.shapeExpr)})"
        case sr: ShapeRef       => s"@${schema.prefixMap.qualify(sr.reference.toRDFNode)}"
        case nc: NodeConstraint => s"NodeConstraint:${nc.show}"
        case s: Shape           => showShape(s) 
        case se: ShapeExternal  => s"External"
      })

  private def showExtends(m: Option[List[ShapeLabel]]) = m match {
      case None => ""
      case Some(es) => s"extends ${es.map(_.show).mkString(",")}"
  }      

  private def showTripleExpr(m: Option[TripleExpr]) = m match {
      case None => ""
      case Some(te) => s"${te.show}"
  }

  def showShape(s: Shape): String = {
    s"Shape(${showExtends(s._extends)}${showTripleExpr(s.expression)})"
  }

}
