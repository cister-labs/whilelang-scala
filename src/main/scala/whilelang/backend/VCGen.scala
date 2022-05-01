package whilelang.backend

import whilelang.syntax.Program.{BExpr, Command, IExpr}
import Command.*
import BExpr.*
import IExpr.*

object VCGen:
  type Subs = Map[String,IExpr]

  def apply(c:Command): Set[BExpr] = vcgen(c)(using BTrue).filterNot(_==BTrue)

  def vcgen(c:Command)(using q:BExpr): Set[BExpr] = c match
    case Skip => Set()
    case Assign(_, _) => Set()
    case Seq(c1, c2) => vcgen(c1)(using wprec(c2)) ++ vcgen(c2)
    case ITE(_, ct, cf) => vcgen(ct) ++ vcgen(cf)
    case While(b, c, inv) => vcgen(c)(using inv) ++
      Set(imply(and(inv,b), wprec(c)),
          imply(and(inv,not(b)), q))
    case Assert(b) => Set(b)
    case Fail => sys.error("fail VCGen")
    case Contract(pre,c,pos) => vcgen(c) + imply(pre,wprec(c)(using and(q,pos)))
      // if q=true, then
      // Contract(true,c,q2) => vcgen(c)(true) + wprec(c)(q2)
      // Seq(c,assert(q2))   => vcgen(c)(wprec(assert(q2))) + vcgen(assert(q2))
      //                      = vggen(c)(q2) + {q2}
      //                maybe?: wprec(assert(b)(q)) = q,  vcgen(assert(q2)) = wprec(

  def wprec(c:Command)(using q:BExpr): BExpr = c match
    case Skip => q
    case Assign(x, e) => subst(q)(using Map(x->e))
    case Seq(c1, c2) => wprec(c1)(using wprec(c2))
    case ITE(b, ct, cf) => and( imply(b, wprec(ct)) , imply(not(b), wprec(cf))  )
    case While(_, _, inv) => inv
    case Assert(b) => and(q,b)
    case Fail => sys.error("fail wprec")
    case Contract(_,c,pos) => wprec(c)(using and(q,pos)) // check

  def subst(b: BExpr)(using q:Subs): BExpr = b match
    case BTrue => b
    case BFalse => b
    case And(b1, b2) => And(subst(b1),subst(b2))
    case Or(b1, b2) => Or(subst(b1),subst(b2))
    case Impl(b1, b2) => Impl(subst(b1),subst(b2))
    case Not(b) => Not(subst(b))
    case Less(e1, e2) => Less(subst(e1),subst(e2))
    case Greater(e1, e2) => Greater(subst(e1),subst(e2))
    case Eq(e1, e2) => Eq(subst(e1),subst(e2))

  def subst(e:IExpr)(using q:Subs): IExpr = e match
    case N(_) => e
    case Var(ident) => q.getOrElse(ident,e)
    case Plus(e1, e2) => Plus(subst(e1),subst(e2))
    case Times(e1, e2) => Times(subst(e1),subst(e2))
    case Minus(e1, e2) => Minus(subst(e1),subst(e2))
