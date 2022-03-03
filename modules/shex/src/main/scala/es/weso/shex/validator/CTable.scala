package es.weso.shex.validator

import cats._
// import com.typesafe.scalalogging.LazyLogging
import es.weso.rbe.interval.{IntLimit, IntOrUnbounded, Unbounded}
import es.weso.rbe.{Direct => _, Schema => _, Star => _, _}
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import es.weso.shex.compact.Parser.TripleExprMap
import cats.implicits._
import io.circe._ 
import io.circe.syntax._
import es.weso.rdf.PrefixMap


/**
  * Constraints table
  *
  * @param constraints
  * @param paths
  * @param elems
  * @param prefixMap
  */
case class CTable(constraints: ConstraintsMap,
                  paths: PathsMap,
                  elems: Int,
                  prefixMap: PrefixMap
                 ) {

    private def addPath(p: Path, n: ConstraintRef): PathsMap =
      paths.updated(p, paths.get(p).getOrElse(Set()) + n)

    def getConstraint(cref: ConstraintRef): Option[(ShapeExpr, Option[List[SemAct]])] = {
      constraints.get(cref).map(ce => ce match {
        case Pos(se,sas) => (se,sas)
        case Neg(se,sas) => (ShapeNot.fromShapeExpr(se),sas)
      })
    }

/*    private lazy val isAmbiguous: Boolean = {
      paths.values.map(_.size).exists(_ > 1)
    } */

    private def addConstraint(path: Path, expr: CheckExpr): (CTable, ConstraintRef) = {
      val cref = ConstraintRef(this.elems, path, path.showQualified(prefixMap))
      val newTable = this.copy(
        elems = this.elems + 1,
        constraints = this.constraints + (cref -> expr),
        paths = addPath(path,cref)
      )
      (newTable,cref)
    }

    def neighs2Candidates(neighs: List[Arc]): Candidates = {
      val rs = neighs.map(arc => Candidate(arc, paths.get(arc.path).getOrElse(Set())))
      Candidates(rs)
    }

    def pathsMap2Json(pair: (Path,Set[ConstraintRef])): Json = {
        val (path,cs) = pair
        Json.obj(
          ("path",Json.fromString(path.toString())),
          ("constraintRef", Json.fromValues(cs.toList.map(_.toString).map(Json.fromString(_)))))
        
    }

    def toJson: Json = Json.obj(
      ("type", Json.fromString("ConstraintTable")),
      ("pathsMap", Json.fromValues(paths.toList.map(pathsMap2Json)))
    )
}


object CTable {

    def empty: CTable = CTable(Map(), Map(), 0, PrefixMap.empty)

    def simplify(rbe: Rbe[ConstraintRef]): Rbe[ConstraintRef] = {
      rbe match {
        case And(Empty, e1) => e1
        case Or(Empty, e1) => e1
        case And(e1, e2) => And(simplify(e1), e2)
        case Or(e1, e2) => Or(simplify(e1), e2)
        case e => e
      }
    }

    private def appearances(iri: IRI, te: TripleExpr): List[ShapeExpr] = te match {
      case tc: TripleConstraint =>
        if (tc.predicate == iri) tc.valueExpr.toList
        else List()
      case eachOf: EachOf =>
        eachOf.expressions.flatMap(appearances(iri,_))
      case oneOf: OneOf =>
        oneOf.expressions.flatMap(appearances(iri,_))
      case i: Inclusion => List() // TODO
      case e: Expr => List()
    }

    private def extendWithExtras(pair: ResultPair, te: TripleExpr, extras: List[IRI]): ResultPair = {
      val zero: ResultPair = pair
      def combine(current: ResultPair, extra: IRI): ResultPair = {
        val s: ShapeExpr = ShapeNot.fromShapeExpr(ShapeOr.fromShapeExprs(appearances(extra, te)))
        val (table,rbe) = current
        val (newTable,cref) = table.addConstraint(Direct(extra),Pos(s,None))
        val newRbe = And(rbe,Symbol(cref,0,Unbounded))
        (newTable,newRbe)
      }
      extras.foldLeft(zero)(combine)
    }

    def mkTable(te: TripleExpr, extras: List[IRI], tripleExprMap: TripleExprMap, prefixMap: PrefixMap): Either[String, ResultPair] = {
      // logger.info(s"mkTable from ${te.id}")
      for {
        pair <- mkTableAux(te, CTable.empty.copy(prefixMap = prefixMap), tripleExprMap)
      } yield extendWithExtras(pair, te, extras)
    }

    // TODO: this method should be tailrec
    private def mkTableAux(te: TripleExpr, current: CTable, teMap: TripleExprMap): Either[String,ResultPair] = {
      te match {
        // TODO: Check if we should add semActs
        case e: EachOf => {
          val zero: Either[String,ResultPair] = Right((current, Empty))
          def comb(rest: Either[String,ResultPair], currentTe: TripleExpr): Either[String,ResultPair] = for {
            pair1 <- rest
            (currentTable, currentRbe) = pair1
            pair2 <- mkTableAux(currentTe, currentTable, teMap)
            (newTable, newRbe) = pair2
          } yield (newTable, And(currentRbe, newRbe))
          for {
            pair <- e.expressions.foldLeft(zero)(comb)
          } yield {
            val (newTable, rbe) = pair
            val simplifiedRbe: Rbe_ = simplify(rbe) // e.expressions.map(_ ).reduce(And)
            val groupRbe = if (Cardinality.isDefault(e.min, e.max)) simplifiedRbe
            else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
            (newTable, groupRbe)
          }
        }
        case e: OneOf => {
          val zero: Either[String,ResultPair] = Right((current, Empty))
          def comb(next: Either[String,ResultPair], currentTe: TripleExpr): Either[String,ResultPair] = for {
            pair1 <- next
            (currentTable, currentRbe) = pair1
            pair2 <- mkTableAux(currentTe, currentTable, teMap)
            (newTable, newRbe) = pair2
          } yield (newTable, Or(currentRbe, newRbe))
          for {
            p <- e.expressions.foldLeft(zero)(comb)
          } yield {
            val (newTable, rbe) = p
            val simplifiedRbe: Rbe_ = simplify(rbe)
            val groupRbe =
              if (Cardinality.isDefault(e.min, e.max))
                simplifiedRbe
              else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
            (newTable, groupRbe)
          }
        }
        case Inclusion(label) => teMap.get(label) match {
          case None => Left(s"Not found value for label $label")
          case Some(te) => mkTableAux(te,current,teMap)
        }
        case tc: TripleConstraint => {
          val valueExpr: CheckExpr = if (tc.negated) {
            tc.valueExpr match {
              case Some(se) => Neg(se,tc.semActs)
              case None => Neg(ShapeExpr.any,tc.semActs)
            }
          } else tc.valueExpr match {
            case Some(se) => Pos(se, tc.semActs)
            case None => Pos(ShapeExpr.any,tc.semActs)
          }
          val (newTable, cref) = current.addConstraint(tc.path, valueExpr)
          val posSymbol = Symbol(cref, tc.min, max2IntOrUnbounded(tc.max))
          val symbol = if (tc.negated) {
            Repeat(posSymbol, 0, IntLimit(1))
          } else posSymbol
          Right((newTable, symbol))
        }

        case e:Expr => {
          val e: Rbe_ = Empty
          // val table = current
          Right((current,e))
        }
      }

    }

    def max2IntOrUnbounded(m: Max): IntOrUnbounded = {
      m match {
        case IntMax(v) => IntLimit(v)
        case Star => Unbounded
      }
    }
  

  implicit lazy val showCTable: Show[CTable] = new Show[CTable] {
    override def show(table: CTable) = {
      def showConstraints(cs: ConstraintsMap): String = {
        def combine(s: List[String], current: (ConstraintRef, CheckExpr)): List[String] = {
          val (cref,expr) = current
          s"${cref.show}->${expr.show}" :: s
        }
        cs.foldLeft(List[String]())(combine).mkString("\n")
      }
     def showPaths(paths: PathsMap): String = 
      paths.map {
        case (path, cr) => s"${path.show}->${cr.map(_.show).mkString(",")}"
      }.mkString(s"\n")

      s"""Constraints:\n${showConstraints(table.constraints)}\nPaths:\n${showPaths(table.paths)}\n---endTable\n""".stripMargin
    }
  }

  implicit val crefKeyEncoder: KeyEncoder[ConstraintRef] = new KeyEncoder[ConstraintRef] {
    override def apply(cref: ConstraintRef): String = cref.toString
  }

  implicit val tableEncoder: Encoder[CTable] = new Encoder[CTable] {
    final def apply(v: CTable): Json = 
      Json.obj(
        ("type", "ConstraintsTable".asJson),
        ("rbe", v.constraints.asJson),
      )
  }
}
