package whilelang.backend

import caos.sos.SOS
import whilelang.backend.SmallSemantics.St
import whilelang.syntax.Program
import whilelang.syntax.Program.BExpr.*
import whilelang.syntax.Program.Command.*
import whilelang.syntax.Program.IExpr.*
import whilelang.syntax.Program.{BExpr, Command, IExpr}

/** Small-step semantics for both commands and boolean+integer expressions.  */
object SmallSemantics extends SOS[String,St]:

  type Env = Map[String,Int]
  type St = (Command,Env)

  /** When is a state terminal */
  override def accepting(s: St): Boolean =
    s._1 == Skip

  /** What are the set of possible evolutions (label and new state) */
  override def next[Act>:String](st: St): Set[(Act, St)] =
    val (comm,env) = st
    comm match
      case Skip => Set()
      case Fail => Set()
      case Seq(Skip,c2) => Set("Skip" -> (c2,env))
      case Seq(c1,c2) =>
        for (by,st) <- next(c1->env) yield
          (by, Seq(st._1,c2)->st._2)
      case While(b,c,i) => Set("while-if" -> (ITE(b,Seq(c,While(b,c,i)),Skip), env))
      case Assert(b) => b match
        case BTrue => Set("assert-true"->(Skip,env))
        case BFalse => Set("assert-false"->(Fail,env))
        case _ => nextBool(b)(using env).map((s,b3) =>
          (s,(Assert(b3),env))).toSet
      case ITE(b, ct, cf) => b match
        case BTrue => Set("if-true" -> (ct, env))
        case BFalse => Set("if-false" -> (cf, env))
        case _ => nextBool(b)(using env).map((s, b3) =>
          (s, (ITE(b3, ct, cf), env))).toSet
      case Assign(ident, e) => e match
        case N(n) => Set(s"Assign $ident:=$n" -> (Skip, env + (ident -> n)))
        case _ => nextInt(e)(using env).map((s, e2) =>
          (s, (Assign(ident, e2), env))).toSet
      case Contract(_,c,_) => next(c,env)

  /** Evaluation of the next rewrite for a boolean expression */
  def nextBool(b: BExpr)(using env: Env): Option[(String, BExpr)] = b match
    // nothing to reduce
    case BTrue => None
    case BFalse => None
    // the concrete reductions
    case And(BTrue, b2) => Some("And-true1", b2)
    case And(b1, BTrue) => Some("And-true2", b1)
    case And(BFalse, _) => Some("And-false1", BFalse)
    case And(_, BFalse) => Some("And-false2", BFalse)
    case Or(BFalse, b2) => Some("Or-false1", b2)
    case Or(b1, BFalse) => Some("Or-false2", b1)
    case Or(BTrue, _) => Some("Or-true1", BFalse)
    case Or(_, BTrue) => Some("Or-true2", BFalse)
    case Not(BTrue) => Some("Not-true", BFalse)
    case Not(BFalse) => Some("Not-true", BTrue)
    case Less(N(i1), N(i2)) => if i1 < i2
      then Some("Less-true", BTrue) else Some("Less-false", BFalse)
    case Greater(N(i1), N(i2)) => if i1 > i2
    then Some("Greater-true", BTrue) else Some("Greater-false", BFalse)
    case Eq(N(i1), N(i2)) => if i1 == i2
    then Some("Eq-true", BTrue) else Some("Eq-false", BFalse)
    // the general case
    case Not(b1) => nextBool(b1).map((s, b1b) => (s, Not(b1b)))
    case And(b1, b2) => mbNext(nextBool, And.apply)(b1, b2)
    case Or(b1, b2) => mbNext(nextBool, Or.apply)(b1, b2)
    case Impl(b1, b2) =>  Some("Impl-def", Or(Not(b1),b2))
    case Less(e1, e2) => mbNext(nextInt, Less.apply)(e1, e2)
    case Greater(e1, e2) => mbNext(nextInt, Greater.apply)(e1, e2)
    case Eq(e1, e2) => mbNext(nextInt, Eq.apply)(e1, e2)

  /** Evaluation of the next rewrite for an int expression */
  def nextInt(e: IExpr)(using env: Env): Option[(String, IExpr)] = e match
    // nothing to reduce
    case N(_) => None
    case Var(ident) => if env contains ident
      then Some(s"Var-$ident" -> N(env(ident)))
      else None
    // base cases
    case Plus(N(n1), N(n2)) => Some("Plus", N(n1 + n2))
    case Minus(N(n1), N(n2)) => Some("Minus", N(n1 - n2))
    case Times(N(n1), N(n2)) => Some("Times", N(n1 * n2))
    case Plus(N(0), e) => Some("Plus-0", e)
    case Plus(e, N(0)) => Some("Plus-0", e)
    case Minus(e, N(0)) => Some("Minus-0", e)
    case Times(N(1), e) => Some("Times-1", e)
    case Times(e, N(1)) => Some("Times-1", e)
    case Power(_, N(0)) => Some("x^0", N(1))
    case Power(N(0), _) => Some("0^x", N(0))
    case Power(N(1), _) => Some("1^x", N(1))
    case Power(e, N(1)) => Some("x^1", e)
    // general cases
    case Plus(e1, e2) => mbNext(nextInt, Plus.apply)(e1, e2)
    case Minus(e1, e2) => mbNext(nextInt, Minus.apply)(e1, e2)
    case Times(e1, e2) => mbNext(nextInt, Times.apply)(e1, e2)
    case Power(e1, e2) => mbNext(nextInt, Power.apply)(e1, e2)

  ///////////////
  // Auxiliary //
  ///////////////

  def mbNext[A, B](nxt: A => Option[(String, A)], op: (A, A) => B)(a1: A, a2: A): Option[(String, B)] =
    nxt(a1) match
      case Some(s, a1b) => Some(s, op(a1b, a2))
      case None => for (s, a2b) <- nxt(a2) yield (s, op(a1, a2b))





