package es.weso.rbe.interval

import scala.util.control.NoStackTrace

case class IntOrUnboundedException(msg: String) extends Exception("IntOrUnbounded: " + msg) with NoStackTrace
