package com.digitalasset.fpbud17

import atto._, Atto._, compat.scalaz._

import scalaz.\/
import scalaz.syntax.applicative._

object Parser {
  import com.digitalasset.fpbud17.{ Scheme => S }

  def parseExpr(inp: String): String \/ S.Expr = expr.parseOnly(inp).either

  lazy val symbol = oneOf("!$%&|*+-/:<=?>@^_~#").named("symbol")
  lazy val spaces = skip(_.isWhitespace)

  lazy val parseString: Parser[S.Expr] = stringLiteral
    .named("string")
    .map(S.String)

  lazy val parseNumber: Parser[S.Expr] = bigInt
    .named("number")
    .map(S.Number)

  lazy val parseAtom: Parser[S.Expr] =
    ((letter | symbol) |@| stringOf(letterOrDigit | symbol))(_ +: _).map[S.Expr] {
        case "#t" => S.Bool(true)
        case "#f" => S.Bool(false)
        case a    => S.Atom(a)
    } named "atom"


  lazy val parseList: Parser[S.Expr] = sepBy(expr, whitespace).map[S.Expr](S.List).named("list")
  lazy val parseDottedPair: Parser[S.Expr] = {
    val p: Parser[S.Expr] = (token(expr) |@| (token(char('.')) ~> expr)) {
      case (head, tail) => S.Pair(head, tail)
    }
    p named "dotted-pair"
  }

  lazy val lists: Parser[S.Expr] = token(char('(')) ~> token(parseDottedPair | parseList) <~ char(')')

  lazy val expr: Parser[S.Expr] = (parseNumber | parseString | parseAtom | lists).named("expr")
}
