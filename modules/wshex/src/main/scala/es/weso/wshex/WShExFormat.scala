package es.weso.wshex

sealed trait WShExFormat
case object CompactFormat extends WShExFormat 
case object JSONFormat extends WShExFormat