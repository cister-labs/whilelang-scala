package whilelang.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.{Visualize, Widget, steps, view}
import caos.view.{Mermaid, Text, View}
import caos.view.Mermaid
import whilelang.syntax.Show
import whilelang.syntax.Program
import whilelang.syntax.Program.Command

object CaosConfig extends Configurator[Command]:
  val name = "WhileLang"

  val parser: String=>Command =
    whilelang.syntax.Parser.parseProgram

  // import whilelang.syntax.Parser.Examples
  val examples = List(
     Example("x:=27; while x>1 do x:=x-5","mod 5","Keeps subtracting 5")
  )

  val widgets: Iterable[(String,Widget[Command])] = List(
    "Parsed data" -> view(_.toString , Text),
    "Pretty" -> view(Show.apply , Text),
    "Run" -> steps(com=>(com,Map()),whilelang.backend.Semantics,_.toString,Text),
    "Run pretty" -> steps(com=>(com,Map()),whilelang.backend.Semantics,
      x=>Show(x._1)+" - "+x._2.mkString(","),Text)

  )