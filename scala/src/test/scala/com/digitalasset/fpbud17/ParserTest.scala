package com.digitalasset.fpbud17

import org.scalatest.{FunSpec, Matchers}

import com.digitalasset.fpbud17.{Parser => P}
import com.digitalasset.fpbud17.{Scheme => S}

import scalaz.\/-

class ParserTest extends FunSpec with Matchers {

  describe("The Scheme parser") {
    it("should parse primitives") {
      P.parseExpr(""""this is a string"""") shouldBe \/-(S.String("this is a string"))
      P.parseExpr("#t") shouldBe \/-(S.Bool(true))
      P.parseExpr("#f") shouldBe \/-(S.Bool(false))
      P.parseExpr("quote") shouldBe \/-(S.Atom("quote"))
      P.parseExpr("123") shouldBe \/-(S.Number(BigInt(123)))
      P.parseExpr("-123") shouldBe \/-(S.Number(BigInt("-123")))

      P.parseExpr("(a test)") shouldBe \/-(S.List(List(S.Atom("a"), S.Atom("test"))))
      P.parseExpr("( a test )") shouldBe \/-(S.List(List(S.Atom("a"), S.Atom("test"))))

      P.parseExpr("(a (nested) test)") shouldBe \/-(S.List(List[S.Expr](S.Atom("a"), S.List(List(S.Atom("nested"))), S.Atom("test"))))
      P.parseExpr("( a (nested) test )") shouldBe \/-(S.List(List[S.Expr](S.Atom("a"), S.List(List(S.Atom("nested"))), S.Atom("test"))))

      P.parseExpr("(dotted . list)") shouldBe \/-(
        S.Pair(S.Atom("dotted"), S.Atom("list")),
      )

      P.parseExpr("(a ( dotted . list ) test)") shouldBe \/-(
        S.List(List[S.Expr](
          S.Atom("a"),
          S.Pair(S.Atom("dotted"), S.Atom("list")),
          S.Atom("test")
        ))
      )
    }

    it("should fail on empty input") {
      P.parseExpr("") shouldBe 'left
    }
  }

}
