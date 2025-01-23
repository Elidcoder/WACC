package wacc.semantic

import wacc.ast.*  

class Context (val b: String, val env: Environment) {

    def contains(v: String): Boolean = env.contains(v)

    def add(v: String, t: Type): Unit = {
        env += (v -> t)
    }

    def getType(v: String): Option[Type] = env.get(v)

    def removeOption(v: String): Option[Environment] = env match {
        case m if m.contains(v) => Some(m -= v)
        case _ => None
    }

    def remove(v: String): Environment = env -= v

    def getArrayElemType(v: String, d: Int): Option[Type] = 
        def go(t: Type, d: Int): Option[Type] = d match {
            case 0 => Some(t)
            case _ => t match {
                case ArrayT(t) => go(t, d - 1)
                case _ => None
            }
        }
        env.get(v) match {
            case Some(t) => go(t, d)
            case _ => None
    }

}
