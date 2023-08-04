package whilelang.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.view.{Code, Mermaid, Text, View}
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
    "mod 5" ->
      "x:=27; while x>5 do x:=x-5;" ->
      "Keeps subtracting 5",
    "if-then-else" ->
      "x:=5*2+10;\nif x<10\nthen {skip;x:=x+20; x:=2*x;}\nelse x:=x*(0-1);",
    "asserts" ->
      "x:=5;\nassert x<8;\nx:=3;\nassert (x>=5);\nx:=0;",
    "Ex5.5" ->
      "if x>0 then {x:=2*x;\n   while x<10 do x:=2*x; }\nelse skip;" ->
      "From RSD book",
    "Sort2" ->
      "if x<=y then { z:=x ; w:=y; } else { w:=x ; z:=y; }" ->
      "Example 5.7 from RSD book",
    "Contract (:=)" ->
      "{x = 5 && y = 10}\naux := y ;\ny := x ;\nx := x + aux;\n{x > 10 && y = 5}" ->
      "Simple contract, used in the lecture slides",
    "Contract (while)" ->
      "{true}\nx:=5;\nwhile (x!=2 && x!=4) {x>0} do {\n  x:=x-1;\n}\n{x!=2}",
    "Ex.1" ->
      "{i = 5}\na := i + 2;\n{(a = 7) && (i = 5)}" ->
      "// description: Ex.1 from the exercises list.",
    "Ex.2" ->
      "{i = 5}\na := i + 2;\n{(a = 7) && (i > 0)}" ->
      "// description: Ex.2 from the exercises list.",
    "Ex.3" ->
      "{(i = 5) && (a = 3)}\na := i + 2;\n{a = 7}" ->
      "// description: Ex.3 from the exercises list.",
    "Ex.4" ->
      "{a = 7}\ni := i + 2;\n{a = 7}" ->
      "// description: Ex.4 from the exercises list.",
    "Ex.5" ->
      "{i = a - 1}\ni := i + 2;\n{i = a + 1}" ->
      "// description: Ex.5 from the exercises list.",
    "Ex.6" ->
      "{true}\na := i + 2;\n{a = i + 2}" ->
      "// description: Ex.6 from the exercises list.",
    "Ex.7" ->
      "{a > b}\nm := 1; n := a - b;\n{m * n > 0}" ->
      "// description: Ex.7 from the exercises list.",
    "Ex.8" ->
      "{s = 2^i}\ni := i + 1; s := s * 2;\n{s = 2^i}" ->
      "// description: Ex.8 from the exercises list.",
    "Ex.9" ->
      "{true}\nif(i < j) then min := i; else min := j;\n{(min <= i) && (min <= j)}" ->
      "// description: Ex.9 from the exercises list.",
    "Ex.10" ->
      "{(i > 0) && (j > 0)}\nif(i < j) then min := i; else min := j;\n{min > 0}" ->
      "// description: Ex.10 from the exercises list.",
    "Ex.11" ->
      "// replace \"?\" with some formula\n{s = 2^i}\nwhile i < n {?} do i := i + 1; s := s * 2;\n{s = 2^i}" ->
      "// description: Ex.11 from the exercises list.",
    "Ex.12" ->
      "// replace \"?\" with some formula\n{(s = 2^i) && (i <= n)}\nwhile i < n {?} do i := i + 1; s := s * 2;\n{s = 2*n}" ->
      "// description:Ex.12: from the exercises list.",
  )

  override val smallWidgets = List(
    "Parsing works" -> check(_ => Nil),
    "View pretty data" -> view(Show.apply , Code("clike")),
  )

  val widgets = List(
//    "Parsing works" -> check(_ => (Nil,Nil)),
//    "View parsed data" -> view(_.toString , Text),
//    "VCGen" -> view(x => VCGen(x).map(Show.apply).mkString("\n"), Text),
    "WPrec" -> view(x => Show(VCGen.wprec(x)(using Program.BExpr.BTrue)), Text),
    "Stepwise: big-step semantics" -> steps(
      com=>(com,Map()), SmallBigSemantics,
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"),
      Text),
    "Stepwise: partial semantics" -> steps(
      com=>(com,Map()), PartialSemantics,
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"),
      Text),
    "Stepwise: small-step semantics" -> steps(
      com=>(com,Map()), // build initial state from a program
      SmallSemantics, // which SOS semantics to use
      (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"), // how to represent the state
      Text // represent as text or as mermaid diagram
    ),
    "Stepwise: small-step semantics (graph)" -> ltsExplore(
      com=>(com,Map()), // build initial state from a program
      SmallSemantics, // which SOS semantics to use
      x=>Show(x._1)+"\n\n"+x._2.mkString("[",",","]"),
      _.toString
      // (nxt,state) => Show(nxt)+"\t\t"+state.mkString("[",",","]"), // how to represent the state
    ),
    "All-steps: big-step semantics" -> lts(
      com=>(com,Map()), SmallBigSemantics, x=>Show(x._1)+"\n\n"+x._2.mkString("[",",","]"), _.toString),
    "All-steps: partial semantics" -> lts(
      com=>(com,Map()), PartialSemantics, x=>Show(x._1)+"\n\n"+x._2.mkString("[",",","]"), _.toString),
    "All-steps: small-step semantics" -> lts(
      com=>(com,Map()), SmallSemantics, x=>Show(x._1)+"\n\n"+x._2.mkString("[",",","]"), _.toString)
  )