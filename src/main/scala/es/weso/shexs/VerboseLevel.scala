package es.weso.shexs

import com.monovore.decline.Opts
import cats.data.Validated._
import cats.kernel.Order


sealed abstract class VerboseLevel {
    val level: Int
    def asBoolean = level > 0
}


object VerboseLevel {

  case object Nothing extends VerboseLevel { val level = 0 }
  case object Basic extends VerboseLevel { val level = 10 }
  case object All extends VerboseLevel { val level = 20 }

  def verboseLevel: Opts[VerboseLevel] = 
        Opts.option[String](
         "verbose", 
         short = "v", 
         help = "verbose level (0 = no info, 1 = basic, 2 = all)")
         .mapValidated(n => n.toLowerCase() match {
             case "0" => valid(Nothing)
             case "1" => valid(Basic)
             case "2" => valid(All)
             case "Nothing" => valid(Nothing)
             case v => invalidNel(s"Unknown value for verbose level: $v")
         })
         .withDefault(Nothing)

  implicit val OrderVerbose: Order[VerboseLevel] = new Order[VerboseLevel] {
    def compare(x: VerboseLevel, y: VerboseLevel) = 
      Order[Int].compare(x.level,y.level)
  }


}