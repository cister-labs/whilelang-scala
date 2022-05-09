package whilelang.syntax

/**
 * Internal structure to represent commands in a simple while language
 * @author José Proença
 */

object Program:

  /** A command in while language */
  enum Command:
    case Skip
    case Seq(c1:Command, c2:Command)
    case Assign(ident:String, e:IExpr)
    case ITE(b:BExpr, ct:Command, cf:Command)
    case While(b:BExpr, c:Command, inv:BExpr)
    case Assert(b:BExpr)
    case Fail
    case Contract(pre:BExpr,c:Command,pos:BExpr)
  
  /** An integer expression in the while language */
  enum IExpr:
    case N(n:Int)
    case Var(ident:String)
    case Plus(e1:IExpr, e2:IExpr)
    case Times(e1:IExpr, e2:IExpr)
    case Minus(e1:IExpr, e2:IExpr)
    case Power(e1:IExpr, e2:IExpr)

  /** A boolean expression in the while language */
  enum BExpr:
    case BTrue
    case BFalse
    case And(b1:BExpr, b2:BExpr)
    case Or(b1:BExpr, b2:BExpr)
    case Not(b:BExpr)
    case Less(e1:IExpr, e2:IExpr)
    case Greater(e1:IExpr, e2:IExpr)
    case Eq(e1:IExpr, e2:IExpr)
    case Impl(e1:BExpr, e2:BExpr)

  object BExpr:
    def imply(b1:BExpr,b2:BExpr): BExpr = (b1,b2) match //Or(Not(b1),b2)
      case (BTrue,_) => b2
      case (_,BFalse) => not(b1)
      case (BFalse,_)|(_,BTrue) => BTrue
      case _ => Impl(b1,b2)

    def and(b1:BExpr,b2:BExpr): BExpr = (b1,b2) match
      case (BTrue,_) => b2
      case (_,BTrue) => b1
      case (BFalse,_)|(_,BFalse) => BFalse
      case _ => And(b1,b2)
    def or(b1:BExpr,b2:BExpr): BExpr = (b1,b2) match
      case (BFalse,_) => b2
      case (_,BFalse) => b1
      case (BTrue,_)|(_,BTrue) => BTrue
      case _ => Or(b1,b2)
    def not(b1:BExpr): BExpr = b1 match
      case BTrue => BFalse
      case BFalse => BTrue
      case _ => Not(b1)


  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////

  object Examples:
    import Program.Command._
    import Program.IExpr._
    import Program.BExpr._

    val p1: Command =
      Seq(
        Assign("x",N(27)),
        While( Less(N(5),Var("x")), Assign("x",Minus(Var("x"),N(5))), BTrue)
      )

