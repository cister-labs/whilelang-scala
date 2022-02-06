package whilelang.syntax

import whilelang.syntax.Program
import whilelang.syntax.Program.{BExpr, Command, IExpr}
import Command.*


object Show:
  def apply(com: Command): String = com match
    case Skip => "skip"
    case Seq(c1, c2) => s"${apply(c1)};\n${apply(c2)}"
    case Assign(ident, e) => s"$ident:=$e"
    case ITE(b, ct, cf) => s"if $b then\n${indent(apply(ct))}\nelse\n${indent(apply(cf))}"
    case While(b, c) => s"while $b do\n${indent(apply(c))}"


  def indent(str:String,n:Int=1): String =
    val s = "  ".repeat(n)
    s + str.replaceAll("\n",s"\n${s}")

