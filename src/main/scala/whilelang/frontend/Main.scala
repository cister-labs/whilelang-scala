package whilelang.frontend

import caos.frontend.Site.initSite
import whilelang.syntax.{Parser, Program}
import whilelang.frontend.CaosConfig
import whilelang.syntax.Program.Command

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[Command](CaosConfig)
}