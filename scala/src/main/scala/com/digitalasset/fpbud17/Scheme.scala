package com.digitalasset.fpbud17

object Scheme {
  def list(exprs: Expr*): List = List(exprs.toList)

  sealed trait Expr
  final case class Atom(value: scala.Predef.String)                    extends Expr
  final case class List(values: scala.collection.immutable.List[Expr]) extends Expr
  final case class Pair(head: Expr, tail: Expr)                        extends Expr
  final case class Number(value: BigInt)                               extends Expr
  final case class String(value: scala.Predef.String)                  extends Expr
  final case class Bool(value: Boolean)                                extends Expr
}
