package whilelang.syntax


object Program:

  enum Command:
    case Skip
    case Seq(c1:Command, c2:Command)
    case Assign(ident:String, e:IExpr)
    case ITE(b:BExpr, ct:Command, cf:Command)
    case While(b:BExpr, c:Command)

  enum IExpr:
    case N(n:Int)
    case Var(ident:String)
    case Plus(e1:IExpr, e2:IExpr)
    case Times(e1:IExpr, e2:IExpr)
    case Minus(e1:IExpr, e2:IExpr)

  enum BExpr:
    case BTrue
    case BFalse
    case And(b1:BExpr, b2:BExpr)
    case Or(b1:BExpr, b2:BExpr)
    case Not(b:BExpr)
    case Less(e1:IExpr, e2:IExpr)
    case Greater(e1:IExpr, e2:IExpr)
    case Eq(e1:IExpr, e2:IExpr)


  ///////////////////
  // Experimenting //
  ///////////////////

  object Examples:
    import Program.Command._
    import Program.IExpr._
    import Program.BExpr._

    val p1 =
      Seq(
        Assign("x",N(27)),
        While( Less(N(1),Var("x")), Assign("x",Minus(Var("x"),N(5))))
      )

