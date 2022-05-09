package whilelang.backend

import whilelang.syntax.{Program, Show}
import whilelang.syntax.Program.{BExpr, Command, IExpr}
import Command.*
import SmallBigSemantics.St
import caos.sos.SOS
import BExpr.*
import IExpr.*

/** Small-step semantics for commands, and big-step semantics
 * for boolean/integer expressions.  */
object SmallBigSemantics extends SOS[String,St]:

  type Env = Map[String,Int]
  type St = (Command,Env)

  /** When is a state terminal */
  override def accepting(s: St): Boolean =
    s._1 == Skip

  /** What are the set of possible evolutions (label and new state) */
  override def next(st: St): Set[(String, St)] = st._1 match
    case Skip => Set()
    case Fail => Set()
    case Seq(Skip,c2) => next(c2->st._2)
    case Seq(c1,c2) =>
      for (by,st) <- next(c1->st._2) yield
        (by, Seq(st._1,c2)->st._2)
    case While(b,c,i) =>
      if eval(b,st._2)
      then Set("while-true"->(Seq(c,While(b,c,i)),st._2))
      else Set("while-false"->(Skip,st._2))
    case Assert(b) =>
      if eval(b,st._2)
      then Set("assert-true"->(Skip,st._2))
      else sys.error(s"assert failed (${Show(b)})")
    case ITE(b,ct,cf) =>
      if eval(b,st._2)
      then Set("if-true"->(ct,st._2))
      else Set("if-false"->(cf,st._2))
    case Assign(ident,e) =>
      val v = eval(e,st._2)
      Set(s"Assign $ident:=$v" -> (Skip,st._2+(ident->v)))
    case Contract(p,c,q) => next(c,st._2)

  /** Evaluation of boolean expressions */
  def eval(b:BExpr,env:Env): Boolean = b match
    case BTrue  => true
    case BFalse => false
    case And(b1, b2)     => eval(b1,env) && eval(b2,env)
    case Or(b1, b2)      => eval(b1,env) || eval(b2,env)
    case Impl(b1, b2)    => !eval(b1,env) || eval(b2,env)
    case Not(b)          => !eval(b,env)
    case Less(e1, e2)    => eval(e1,env) < eval(e2,env)
    case Greater(e1, e2) => eval(e1,env) > eval(e2,env)
    case Eq(e1, e2)      => eval(e1,env) == eval(e2,env)

  /** Evaluation of integer expressions */
  def eval(e:IExpr,env:Env): Int = e match
    case N(n)       => n
    case Var(ident) =>
      if env contains ident
        then env(ident)
        else sys.error(s"Variable $ident not found.")
    case Plus(e1, e2)  => eval(e1,env) + eval(e2,env)
    case Times(e1, e2) => eval(e1,env) * eval(e2,env)
    case Minus(e1, e2) => eval(e1,env) - eval(e2,env)
    case Power(e1, e2) => math.pow(eval(e1,env) , eval(e2,env)).toInt



