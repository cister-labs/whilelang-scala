package whilelang.backend

import caos.sos.SOS
import whilelang.backend.PartialSemantics.St
import whilelang.syntax.Program.{BExpr, Command, IExpr}
import Command.*
import IExpr.*
import BExpr.*
import whilelang.syntax.Show

/** Small-step semantics for commands, big-step for expressions,
 * with partial evaluation. I.e., a boolean expression with unknown variables
 * can be evaluated to either cases. */
object PartialSemantics extends SOS[String,St]:

  type Env = Map[String,Int]
  type St = (Command,Env)

  /** When is a state terminal */
  override def accepting(s: St): Boolean =
    s._1 == Skip

  /** What are the set of possible evolutions (label and new state) */
  override def next[Act>:String](st: St): Set[(Act, St)] = st._1 match
    case Skip => Set()
    case Fail => Set()
    case Seq(Skip,c2) => next(c2->st._2)
    case Seq(c1,c2) =>
      for (by,st) <- next(c1->st._2) yield
        (by, Seq(st._1,c2)->st._2)
    case While(b,c,i) => eval(b,st._2) match
      case None => Set(s"${Show(b)}-true?" ->(Seq(c,While(b,c,i)),st._2),
                          s"${Show(b)}-false?"->(Skip,st._2))
      case Some(true)  => Set("while-true" ->(Seq(c,While(b,c,i)),st._2))
      case Some(false) => Set("while-false"->(Skip,st._2))
    case Assert(b) => eval(b,st._2) match
      case None => Set()
      case Some(true) => Set("assert-true"->(Skip,st._2))
      case Some(false) => Set("assert-false"->(Fail,st._2))
    case ITE(b,ct,cf) => eval(b,st._2) match
      case None => Set(s"${Show(b)}-true?" ->(ct,st._2),
                          s"${Show(b)}-false?"->(cf,st._2))
      case Some(true)  => Set("if-true" ->(ct,st._2))
      case Some(false) => Set("if-false"->(cf,st._2))
    case Assign(ident,e) => eval(e,st._2) match
      case None  => Set(s"Some assign $ident:=${Show(e)}" -> (Skip,st._2))
      case Some(v) => Set(s"Assign $ident:=$v" -> (Skip,st._2+(ident->v)))
    case Contract(_,c,_) => next(c,st._2)

  /** Evaluation of integer expressions */
  def eval(e:IExpr,env:Env): Option[Int] = e match
    case N(n)       => Some(n)
    case Var(ident) => if env contains ident
      then Some(env(ident))
      else None
    case Plus(e1, e2)  => for (x<-eval(e1,env);y<-eval(e2,env)) yield x+y
    case Times(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x*y
    case Minus(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x-y
    case Power(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield math.pow(x,y).toInt

  /** Evaluation of boolean expressions */
  def eval(b:BExpr,env:Env): Option[Boolean] = b match
    case BTrue  => Some(true)
    case BFalse => Some(false)
    case And(b1, b2)     => for (x<-eval(b1,env);y<-eval(b2,env)) yield x&&y
    case Or(b1, b2)      => for (x<-eval(b1,env);y<-eval(b2,env)) yield x||y
    case Impl(b1, b2)    => for (x<-eval(b1,env);y<-eval(b2,env)) yield (!x)||y
    case Not(b1)         => for x<-eval(b1,env) yield !x
    case Less(e1, e2)    => for (x<-eval(e1,env);y<-eval(e2,env)) yield x<y
    case Greater(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x>y
    case Eq(e1, e2)      => for (x<-eval(e1,env);y<-eval(e2,env)) yield x==y