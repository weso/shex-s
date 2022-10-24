package es.weso.wbmodel

sealed abstract class Rank
case object Preferred extends Rank
case object Normal extends Rank
case object Deprecated extends Rank

