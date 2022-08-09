package es.weso

import es.weso.rbe.interval.{ IntOrUnbounded, IntLimit}
package object wshex {

    type Min = Int 
    type Max = IntOrUnbounded
    
    lazy val defaultMin = 1
    lazy val defaultMax = IntLimit(1)
  
}
