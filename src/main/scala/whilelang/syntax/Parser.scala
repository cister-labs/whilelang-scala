package whilelang.syntax

import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Numbers.*
import cats.syntax.all.*
import P.*
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import whilelang.syntax.Program.{BExpr, Command, IExpr}
import whilelang.syntax.Program.Command.*
import whilelang.syntax.Program.BExpr.*
import whilelang.syntax.Program.IExpr.*

import scala.sys.error

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):Command =
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A],str:String): Either[String,A] =
    parser.parseAll(str) match //.fold(e=>prettyError(str,e), x=>x)
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  def prettyError(str:String,err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${"-".repeat(y+1)+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  def symbols: P[String] = // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as sybmols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string


  /** A program is a command with possible spaces or comments around. */
  def program: P[Command] = contract.surroundedBy(sps)

  def contract: P[Command] =
    (invariant~command.surroundedBy(sps)~invariant)
      .map(x => Contract(x._1._1, x._1._2, x._2)) |
    command

  /** (Recursive) Parser for a command in the while language */
  def command: P[Command] = P.recursive(commRec =>
    def basicCommand:P[Command] =
      skip | ite | whilec | assert | assign
//      contract | skip | ite | whilec | assert | assign

    def skip: P[Skip.type] =
//      string("skip").as(Skip)
      (string("skip")~sps~char(';')).as(Skip)
    def ite: P[ITE] =
      (string("if") ~ bexpr.surroundedBy(sps) ~
        string("then") ~ commBlock.surroundedBy(sps) ~
        string("else") ~ sps ~ commBlock)
        .map(x => ITE(x._1._1._1._1._1._2, x._1._1._1._2, x._2))
    def whilec: P[While] =
      (string("while") ~ bexpr.surroundedBy(sps) ~ invariant.? ~ sps ~
        string("do") ~ sps ~ commBlock)
        .map(x =>
          While(x._1._1._1._1._1._2, x._2, x._1._1._1._1._2.getOrElse(BTrue)) )
    def assert: P[Assert] =
      (string("assert") *> bexpr.surroundedBy(sps)<*char(';'))
        .map(Assert.apply)
    def commBlock =
      (char('{')~sps)*>commRec<*(sps~char('}')) |
        skip  | assert | assign
    def assign: P[Assign] =
//      (varName ~ (string(":=")|char('=')).surroundedBy(sps) ~ iexpr)
      (varName ~ (string(":=")|char('=')).surroundedBy(sps) ~ iexpr <*(sps~char(';')))
        .map(x => Assign(x._1._1,x._2))
//    def contract: P[Command] = // contracts now only in the outside, after ';' is at the end of every assignment.
//      (invariant~commRec.surroundedBy(sps)~invariant)
//        .map(x => Contract(x._1._1, x._1._2, x._2))

    def seqOp =
      char(';').as(Seq.apply)

    def seqOp2 =
      sps.as(Seq.apply)

//    listSep(basicCommand, seqOp2)
//    listSepRep(basicCommand, char(';').surroundedBy(sps), Seq.apply)
    listSepRep(basicCommand, sps, Seq.apply)
  )

  def invariant: P[BExpr] =
    char('{')*>bexpr.surroundedBy(sps)<*char('}')

  /** (Recursive) Parser for a boolean expression */
  def bexpr: P[BExpr] = P.recursive( bexprRec =>
    def lit:P[BExpr] = P.recursive( litR =>
      string("true").as(BTrue) |
      string("false").as(BFalse) |
      (char('!')*>litR).map(Not.apply) |
      ineq.backtrack |
      char('(') *> bexprRec <* char(')')
    )
    def insideBrackets:P[BExpr] =
      bexprRec.backtrack | ineq
    def op:P[(IExpr,IExpr)=>BExpr] =
      string("<=").as((x:IExpr,y:IExpr) => Or(Less(x,y),Eq(x,y))) |
      string(">=").as((x:IExpr,y:IExpr) => Or(Greater(x,y),Eq(x,y))) |
      string("!=").as((x:IExpr,y:IExpr) => Not(Eq(x,y))) |
      char('<').as(Less.apply) |
      char('>').as(Greater.apply) |
      char('=').as(Eq.apply)
    def ineq =
      (iexpr ~ op.surroundedBy(sps) ~ iexpr).map(x=>x._1._2(x._1._1,x._2))
    def or: P[(BExpr,BExpr)=>BExpr] =
      (string("||")|string("\\/")).map(_ => Or.apply)
    def and: P[(BExpr,BExpr)=>BExpr] =
      (string("&&")|string("/\\")).map(_ => And.apply)

    listSep( listSep(lit,and) , or)
  )

  /** (Recursive) Parser for an integer expression */
  def iexpr: P[IExpr] = P.recursive( iexprRec =>
    def lit:P[IExpr] =
      char('(') *> iexprRec.surroundedBy(sps) <* char(')') |
      digits.map(x=>N(x.toInt)) |
      varName.map(Var.apply)
    def pow: P[(IExpr,IExpr)=>IExpr] =
      string("^").map(_ => Power.apply)
    def mult: P[(IExpr,IExpr)=>IExpr] =
      string("*").map(_ => Times.apply)
    def plusminus: P[(IExpr,IExpr)=>IExpr] =
      string("+").as(Plus.apply) |
      string("-").as(Minus.apply)

    listSep( listSep(listSep(lit,pow),mult) , plusminus )
  )


  /// Auxiliary parser combinators

  /** Non-empty list of elements with a binary operator */
  def listSep[A](elem:P[A],op:P0[(A,A)=>A]): P[A] =
    (elem ~ (op.surroundedBy(sps).backtrack.with1~elem).rep0)
      .map(x=>
        val pairlist = x._2
        val first = x._1;
        pairlist.foldLeft(first)((rest,pair) => pair._1(rest,pair._2))
      )

  /** Pair of elements with a separator */
  def binary[A,B](p1:P[A],op:String,p2:P[B]): P[(A,B)] =
    (p1 ~ string(op).surroundedBy(sps) ~ p2).map(x=>(x._1._1,x._2))

  /** Similar to `listSep`, but using native repSep that is more eager (less backtrack). */
  def listSepRep[A](elem:P[A],sep:P0[_],join:(A,A)=>A): P[A] =
    elem.repSep(sep)
      .map(ls => ls.tail.fold(ls.head)(join(_, _)))


  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////
  object Examples:
    val ex1 =
      """x:=28; while(x>1) do x:=x-1"""
