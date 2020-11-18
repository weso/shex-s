package es.weso.shex.validator
import cats.effect._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import ShExChecker._
import es.weso.shex._
import es.weso.shapeMaps.ShapeMapLabel


object ValidationUtils {

  private lazy val `sh:targetNode` = sh + "targetNode"


  def mkSeq[A,B](vs: List[A], f: A => IO[List[B]]): IO[List[B]] = {
    vs.traverse(f).map((_.flatten))
  }

  def getTargetNodeDeclarations(rdf: RDFReader): Check[List[(RDFNode, ShapeLabel)]] =
    for {
      ts <- fromStream(rdf.triplesWithPredicate(`sh:targetNode`))
      r  <- checkAll(ts.map(t => (t.obj, mkShapeLabel(t.subj))).toList.map(checkPair2nd))
  } yield r

  private def mkShapeLabel(n: RDFNode): Check[ShapeLabel] = {
    n match {
      case i: IRI   => ok(IRILabel(i))
      case b: BNode => ok(BNodeLabel(b))
      case _ => {
        errStr(s"mkShapeLabel: Node ${n.show} can't be a shape")
      }
    }
  }

  lazy val ignoredPathsClosed: List[Path] = List(Inverse(`sh:targetNode`))

  def mkLabel(label: ShapeMapLabel): ShapeLabel =
    ShapeLabel.fromShapeMapLabel(label)

  def getPaths(s: Shape, schema: ResolvedSchema): Check[List[Path]] =
    fromEitherString(s.paths(schema).map(_.toList))


  def infoTyping(t: ShapeTyping, msg: String, shapesPrefixMap: PrefixMap): Check[Unit] = for {
   nodesPrefixMap <- getNodesPrefixMap
   _ <- info(s"$msg: ${t.showShort(nodesPrefixMap,shapesPrefixMap)}")
  } yield ()

  def getNodesPrefixMap: Check[PrefixMap] = for {
    rdf <- getRDF
    pm <- fromIO(rdf.getPrefixMap)
  } yield pm  

}