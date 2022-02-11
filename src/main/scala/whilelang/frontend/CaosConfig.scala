package whilelang.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.{Visualize, Widget, steps, view}
import caos.view.{Mermaid, Text, View}
import caos.view.Mermaid
import whilelang.syntax.Show
import whilelang.syntax.Program
import whilelang.syntax.Program.Command
import whilelang.backend.*

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Command]:
  val name = "WhileLang"

  val parser: String=>Command =
    whilelang.syntax.Parser.parseProgram

  val examples = List(
    Example("x:=27; while x>5 do x:=x-5",
     "mod 5", "Keeps subtracting 5"),
    Example("x:=5*2+10;\nif x<10\nthen {skip;x:=x+20; x:=2*x}\nelse x:=x*(0-1)",
      "if-then-else",""),
    Example("if x>0 then {x:=2*x;\n   while x<10 do x:=2*x }\nelse skip",
      "Ex5.5","From RSD book"),
    Example("if x<=y then { z:=x ; w:=y } else { w:=x ; z:=y }",
      "Sort2","Example 5.7 from RSD book")
  )

  val widgets: Iterable[(String,Widget[Command])] = List(
    "View parsed data" -> view(_.toString , Text),
    "View pretty data" -> view(Show.apply , Text),
    "Run big-steps" -> steps(
      com=>(com,Map()), SmallBigSemantics,
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"),
      Text),
    "Run partial-semantics" -> steps(
      com=>(com,Map()), PartialSemantics,
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"),
      Text),
    "Run small-steps" -> steps(
      com=>(com,Map()), // build initial state from a program
      SmallSemantics, // which SOS semantics to use
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"), // how to represent the state
      Text // represent as text or as mermaid diagram
    )
  )