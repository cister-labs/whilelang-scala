package whilelang.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.view.{Mermaid, Text, View}
import whilelang.backend.*
import whilelang.syntax.Program.Command
import whilelang.syntax.{Program, Show}

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Command]:
  val name = "Animator of a simple While-language"
  override val languageName: String = "WhileLang"

  val parser: String=>Command =
    whilelang.syntax.Parser.parseProgram

  val examples = List(
    "mod 5" -> "x:=27; while x>5 do x:=x-5" ->
      "Keeps subtracting 5",
    "if-then-else" ->
      "x:=5*2+10;\nif x<10\nthen {skip;x:=x+20; x:=2*x}\nelse x:=x*(0-1)",
    "asserts" ->
      "x:=5;\nassert x<8;\nx:=3;\nassert (x>=5);\nx:=0",
    "Ex5.5" -> "if x>0 then {x:=2*x;\n   while x<10 do x:=2*x }\nelse skip" ->
      "From RSD book",
    "Sort2" -> "if x<=y then { z:=x ; w:=y } else { w:=x ; z:=y }" ->
      "Example 5.7 from RSD book"
  )

  val widgets = List(
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
    ),
    "LTS big-steps" -> lts(
      com=>(com,Map()), SmallBigSemantics, x=>Show(x._1), _.toString),
    "LTS partial-semantics" -> lts(
      com=>(com,Map()), PartialSemantics, x=>Show(x._1), _.toString),
    "LTS small-steps" -> lts(
      com=>(com,Map()), SmallSemantics, x=>Show(x._1), _.toString)
  )