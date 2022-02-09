package whilelang.backend

import caos.sos.SOS
import whilelang.backend.MaybeSemantics.St
import whilelang.syntax.Program.{BExpr, Command, IExpr}
import Command.*
import IExpr.*
import BExpr.*
import whilelang.syntax.Show

object MaybeSemantics extends SOS[String,St]:

  type Env = Map[String,Int]
  type St = (Command,Env)

  /** When is a state terminal */
  override def accepting(s: St): Boolean =
    s._1 == Skip

  /** What are the set of possible evolutions (label and new state) */
  override def next(st: St): Set[(String, St)] = st._1 match
    case Skip => Set()
    case Seq(Skip,c2) => next(c2->st._2)
    case Seq(c1,c2) =>
      for (by,st) <- next(c1->st._2) yield
        (by, Seq(st._1,c2)->st._2)
    case While(b,c) => eval(b,st._2) match
      case None => Set(s"${Show(b)}-true?" ->(Seq(c,While(b,c)),st._2),
                          s"${Show(b)}-false?"->(Skip,st._2))
      case Some(true)  => Set("while-true" ->(Seq(c,While(b,c)),st._2))
      case Some(false) => Set("while-false"->(Skip,st._2))
    case ITE(b,ct,cf) => eval(b,st._2) match
      case None => Set(s"${Show(b)}-true?" ->(ct,st._2),
                          s"${Show(b)}-false?"->(cf,st._2))
      case Some(true)  => Set("if-true" ->(ct,st._2))
      case Some(false) => Set("if-false"->(cf,st._2))
    case Assign(ident,e) => eval(e,st._2) match
      case None  => Set(s"Some assign $ident:=${Show(e)}" -> (Skip,st._2))
      case Some(v) => Set(s"Assign $ident:=$v" -> (Skip,st._2+(ident->v)))

  /** Evaluation of integer expressions */
  def eval(e:IExpr,env:Env): Option[Int] = e match
    case N(n)       => Some(n)
    case Var(ident) => if env contains ident
      then Some(env(ident))
      else None
    case Plus(e1, e2)  => for (x<-eval(e1,env);y<-eval(e2,env)) yield x+y
    case Times(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x*y
    case Minus(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x-y

  /** Evaluation of boolean expressions */
  def eval(b:BExpr,env:Env): Option[Boolean] = b match
    case BTrue  => Some(true)
    case BFalse => Some(false)
    case And(b1, b2)     => for (x<-eval(b1,env);y<-eval(b2,env)) yield x&&y
    case Or(b1, b2)      => for (x<-eval(b1,env);y<-eval(b2,env)) yield x||y
    case Not(b1)         => for x<-eval(b1,env) yield !x
    case Less(e1, e2)    => for (x<-eval(e1,env);y<-eval(e2,env)) yield x<y
    case Greater(e1, e2) => for (x<-eval(e1,env);y<-eval(e2,env)) yield x>y
    case Eq(e1, e2)      => for (x<-eval(e1,env);y<-eval(e2,env)) yield x==y