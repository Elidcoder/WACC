package wacc.backend.referencing

import wacc.backend.Context
import wacc.semantic.QualifiedName
import wacc.ast._
import wacc.backend.ir._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.backend.referencing.referencer.getTypeSize

class UnitBackEndTest extends AnyFlatSpec {
    given pos: Pos   = Pos(0,0)
    val initial_off  = 16
    val context      = Context()

    /* Test data for types and values */
    val intType  = IntT()
    val charType = CharT()
    val boolType = BoolT()
    val strType  = StringT()

    /* Test for getTypeSize function */
    it should "correctly determine data size for types" in {
        referencer.getTypeSize(charType) shouldBe BYTE
        referencer.getTypeSize(boolType) shouldBe BYTE
        referencer.getTypeSize(intType)  shouldBe DWORD
        referencer.getTypeSize(strType)  shouldBe QWORD
        referencer.getTypeSize(ArrayT(intType))          shouldBe QWORD
        referencer.getTypeSize(PairT(intType, charType)) shouldBe QWORD
    }

    /* Test addVarToContext works correctly */
    it should "add variables to context with correct offsets" in {
        given KnownType = intType

        /* Create a variable and add it to context. */
        val varName = Ident[QualifiedName, KnownType](QualifiedName("var1", 1))
        referencer.addVarToContext(varName)(using context, QualifiedName("main", -1))

        /* Check that the variable is correctly referenced. */
        try {
            context.getVarRef(varName.name) match {
                case MemOff(BASE_PTR_REG, offset) => offset shouldBe (-getTypeSize(intType).bytes)
                case _ => fail("Reference was badly formatted")
            }
        }
        catch {
            _ => fail("Variable was not added to context correctly.")
        }
    }

    /* Test referencing variables in a list of statements works */
    it should "correctly reference variables in a program" in {
        given KnownType = intType

        /* Create 2 vars. */
        val var1 = Ident[QualifiedName, KnownType](QualifiedName("var1", 1))
        val var2 = Ident[QualifiedName, KnownType](QualifiedName("var1", 2))

        /* Turn them into a list of statents and reference that. */
        val stmt1 = NewAss(IntT(),  var1, CharLit('a'))
        val stmt2 = NewAss(CharT(), var2, CharLit('z'))
        referencer.reference(List(stmt1, stmt2))(using context, QualifiedName("main", -1))

        /* Check the variables were referenced properly */
        try {
            context.getVarRef(var1.name) match {
                case MemOff(BASE_PTR_REG, offset) => offset shouldBe (-getTypeSize(intType).bytes)
                case _ => fail("Var1 reference was badly formatted") 
            }
            context.getVarRef(var2.name) match {
                case MemOff(BASE_PTR_REG, offset) => offset shouldBe (-2 * getTypeSize(intType).bytes)
                case _ => fail("Var2 reference was badly formatted") 
            }
        }
        catch {
            _ => fail("Variables were not both added to context.")
        }
    }

    /* Test referencing works for all types of variables  */
    it should "correctly reference differently typed variables in a program" in {
        given KnownType = intType

        /* Create 2 vars and turn them into statments. */
        val var1 = Ident[QualifiedName, KnownType](QualifiedName("var1", 1))
        val stmt1 = NewAss(IntT(),  var1, IntLit(50))
        {   
            given KnownType = charType
            val var2 = Ident[QualifiedName, KnownType](QualifiedName("var1", 2))
            val stmt2 = NewAss(CharT(), var2, CharLit('z'))
            {
                given KnownType = boolType
                val var3 = Ident[QualifiedName, KnownType](QualifiedName("var3", 3))
                val stmt3 = NewAss(IntT(),  var3, BoolLit(true))
                {
                    given KnownType = strType
                    val var4 = Ident[QualifiedName, KnownType](QualifiedName("var1", 4))
                    val stmt4 = NewAss(CharT(), var4, StrLit("teststring"))

                    /* Turn the statments into a list of statents and reference that. */
                    referencer.reference(List(stmt1, stmt2, stmt3, stmt4))(using context, QualifiedName("main", -1))

                    /* Check the variables were referenced properly */
                    try {
                        var curOff = -getTypeSize(intType).bytes
                        context.getVarRef(var1.name) match {
                            case MemOff(BASE_PTR_REG, offset) => offset shouldBe curOff
                            case _ => fail("Var1 reference was badly formatted") 
                        }
                        curOff -= getTypeSize(charType).bytes
                        context.getVarRef(var2.name) match {
                            case MemOff(BASE_PTR_REG, offset) => offset shouldBe curOff
                            case _ => fail("Var2 reference was badly formatted") 
                        }
                        curOff -= getTypeSize(boolType).bytes
                        context.getVarRef(var3.name) match {
                            case MemOff(BASE_PTR_REG, offset) => offset shouldBe curOff
                            case _ => fail("Var3 reference was badly formatted") 
                        }
                        curOff -= getTypeSize(strType).bytes
                        context.getVarRef(var4.name) match {
                            case MemOff(BASE_PTR_REG, offset) => offset shouldBe curOff
                            case _ => fail("Var4 reference was badly formatted") 
                        }
                    }
                    catch {
                        _ => fail("Variables were not both added to context.")
                    }
                }
            }
        }
    }

    /* Test referencing in if-else statements */
    it should "correctly reference variables in if-else statements" in {
        given KnownType = intType

        /* Create a var and an if statment. */
        val var1 = Ident[QualifiedName, KnownType](QualifiedName("var1", 1))
        val ifStmt:Stmt[QualifiedName, KnownType] = If(IntLit(1), List(NewAss(IntT(), var1, IntLit(42))), List())

        /* Reference the if statment. */
        referencer.reference(List(ifStmt))(using context, QualifiedName("main", -1))
        
        /* Check the variable is referenced properly */
        try {
            context.getVarRef(var1.name) match {
                case MemOff(BASE_PTR_REG, offset) => offset shouldBe (-getTypeSize(intType).bytes)
                case _ => fail("Reference was badly formatted from if-else statement") 
            }
        }
        catch {
            _ => fail("Variable was not added to context.")
        }
    }

    /* Test for referencing function parameters */
    it should "correctly reference function parameters with registers" in {
        given KnownType = boolType

        /* Create a function id, bool var and turn the var into a parameter. */
        val funcID = Ident[QualifiedName, KnownType](QualifiedName("testFunc", 1))
        val var1 = Ident[QualifiedName, KnownType](QualifiedName("var1", 2))
        val param1 = Param[QualifiedName, KnownType](intType, var1)(pos)
        
        /* Create another var and turn it into a parameter. */
        val var2 = Ident[QualifiedName, KnownType](QualifiedName("var2", 3))
        val param2 = Param[QualifiedName, KnownType](intType, var2)(pos)
        

        /* Create a function using the params and reference it. */
        val func = Func[QualifiedName, KnownType](
            boolType,
            funcID,
            List(param1, param2),
            List.empty
        )(pos)
        referencer.reference(func)(using context)

        /* Check parameters have been added to the context to correct registers */
        try {
            context.getVarRef(param1.paramId.name) match {
                case Reg(register) => register shouldBe RDI
                case _ => fail("Param1 reference was badly formatted") 
            }
            context.getVarRef(param2.paramId.name) match {
                case Reg(register) => register shouldBe RSI
                case _ => fail("Param2 reference was badly formatted") 
            }
        }
        catch {
            _ => fail("Parameters were not both added to context.")
        }
    }
}
