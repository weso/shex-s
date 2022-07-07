package es.weso.shapepath
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap

sealed abstract class NodeTest {
  def showQualify(pm: PrefixMap): String = this match {
    case EqName(iri)     => pm.qualify(iri)
    case WildcardTest    => "*"
    case RegExpTest(pat) => s"pattern(${pat})"
    case AnyKindTest     => "expr()"
  }
}
sealed abstract class KindTest extends NodeTest
case class RegExpTest(pattern: String) extends KindTest
case object AnyKindTest extends KindTest

sealed abstract class NameTest extends NodeTest
case class EqName(iri: IRI) extends NameTest
case object WildcardTest extends NameTest
