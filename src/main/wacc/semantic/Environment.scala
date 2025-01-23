package wacc.semantic 

import wacc.ast.Type
import scala.collection.mutable

type Environment = mutable.Map[String, Type]
