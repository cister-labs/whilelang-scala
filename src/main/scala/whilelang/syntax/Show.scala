package whilelang.syntax

import whilelang.syntax.Program
import Program.{BExpr, Command, IExpr}
import Command.*
import IExpr.*
import BExpr.*


/**
 * List of functions to produce textual representations of commands
 */
object Show:
  /** Pretty command, using indentation */
  def apply(com: Command): String = com match
    case Skip => "skip"
    case Seq(c1, c2) => s"${apply(c1)};\n${apply(c2)}"
    case Assign(ident, e) => s"$ident:=${apply(e)}"
    case ITE(b, ct, cf) => s"if ${apply(b)} then\n${indent(apply(ct))}\nelse\n${indent(apply(cf))}"
    case While(b, c, i) => s"while ${apply(b)} ${if i!=BTrue then s"{${apply(i)}} " else ""}do\n${indent(apply(c))}"
    case Assert(b) => s"assert ${apply(b)}"
    case Fail => "FAIL"
    case Contract(pre,c,pos) => s"{${apply(pre)}}\n${apply(c)}\n{${apply(pos)}}"

  def apply(e: IExpr): String = e match
    case N(n) => n.toString
    case Var(ident) => ident
    case Plus(e1, e2) => s"${apply(e1)}+${apply(e2)}"
    case Times(e1, e2) => s"${applyPar(e1)}*${applyPar(e2)}"
    case Minus(e1, e2) => s"${apply(e1)}-${apply(e2)}"
    case Power(e1, e2) => s"${applyLit(e1)}^${applyLit(e2)}"

  def applyPar(expr: IExpr): String = expr match
    case _:(Plus|Minus) => s"(${apply(expr)})"
    case _ => apply(expr)

  def applyLit(expr:IExpr): String = expr match
    case N(_)| Var(_) => apply(expr)
    case _ => s"(${apply(expr)})"

  def apply(b: BExpr): String = b match
    case BTrue => "true"
    case BFalse => "false"
    case Impl(e1, e2) => s"(${apply(e1)}) -> (${apply(e2)})"
    case And(b1, b2) => s"${applyAnd(b1)} && ${applyAnd(b2)}"
    case Or(b1, b2) => s"${apply(b1)} || ${apply(b2)}"
    case Not(b1) => s"!${applyNot(b1)}"
    case Less(e1, e2) => s"${apply(e1)}<${apply(e2)}"
    case Greater(e1, e2) => s"${apply(e1)}>${apply(e2)}"
    case Eq(e1, e2) => s"${apply(e1)}=${apply(e2)}"

  def applyAnd(expr: Program.BExpr): String = expr match
    case _: Or => s"(${apply(expr)})"
    case _ => s"${apply(expr)}"

  def applyNot(expr: Program.BExpr): String = expr match
    case _: (Or | And) => s"(${apply(expr)})"
    case _ => s"${apply(expr)}"

  def indent(str: String, n: Int = 1): String =
    val s = "  ".repeat(n)
    s + str.replaceAll("\n", s"\n$s")

