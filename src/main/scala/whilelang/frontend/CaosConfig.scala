package whilelang.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.{Visualize, Widget, steps, view}
import caos.view.{Mermaid, Text, View}
import caos.view.Mermaid
import whilelang.syntax.Show
import whilelang.syntax.Program
import whilelang.syntax.Program.Command

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Command]:
  val name = "WhileLang"

  val parser: String=>Command =
    whilelang.syntax.Parser.parseProgram

  val examples = List(
     Example("x:=27; while x>5 do x:=x-5",
       "mod 5", "Keeps subtracting 5"),
     Example("x:=5*2+10;\nif x<10\nthen {skip;x:=x+20; x:=2*x}\nelse x:=x*(0-1)",
       "if-then-else","")
  )

  val widgets: Iterable[(String,Widget[Command])] = List(
    "Parsed data" -> view(_.toString , Text),
    "Pretty" -> view(Show.apply , Text),
    "Run" -> steps(com=>(com,Map()),whilelang.backend.Semantics,_.toString,Text),
    "Run pretty" -> steps(com=>(com,Map()),whilelang.backend.Semantics,
      x=>Show(x._1)+"\t\t"+x._2.mkString("[",",","]"),Text)

  )