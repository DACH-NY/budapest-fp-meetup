package com.digitalasset.fpbud17

import com.digitalasset.fpbud17.{Scheme => S}

import Function.const

import scalaz.{@@, Tag, -\/, \/, \/-}
import scalaz.syntax.applicative._

object Eval {

  type Var = String

  sealed trait LambdaTag
  type Lambda = (Var, S.Expr) @@ LambdaTag
  val Lambda                         = Tag.of[LambdaTag]
  def Lam(v: Var, e: S.Expr): Lambda = Lambda((v, e))

  type Env = Var => String \/ DomainValue


  sealed trait DomainValue
  final case class Closure(lambda: Lambda, env: Env)         extends DomainValue
  final case class StringLiteral(value: String)              extends DomainValue
  final case class NumLiteral(value: BigInt)                 extends DomainValue
  final case class Bool(value: Boolean)                      extends DomainValue
  final case class VList(values: List[DomainValue])          extends DomainValue
  final case class VPair(fst: DomainValue, snd: DomainValue) extends DomainValue

  sealed trait Cont
  final case object Empty                         extends Cont
  final case class Ar(c: S.Expr, e: Env, k: Cont) extends Cont
  final case class Fn(l: Lambda, e: Env, k: Cont) extends Cont

  // CEK Machine state
  // C := Expression, E := Env, K := Cont
  type EvalState = (S.Expr, Env, Cont)
  def Right(c: S.Expr, e: Env, k: Cont): String \/ EvalState = \/.right((c, e, k))
  def getExpr(s: EvalState) = s._1
  def getEnv(s: EvalState)  = s._2
  def getCont(s: EvalState) = s._3

  def inject(e: S.Expr): EvalState = (e, const(\/.left(s"not bound value: $e")), Empty)

  def step(e: EvalState): String \/ EvalState = e match {
    case (S.List(S.Atom("lambda") :: S.List(S.Atom(param) :: Nil) :: body :: Nil),
          env,
          Ar(e, envP, cont)) =>
      val lam = Lam(param, S.List(body :: Nil))
      \/.right((e, envP, Fn(lam, env, cont)))

    case (S.List(S.Atom("lambda") :: S.List(S.Atom(param) :: Nil) :: body :: Nil),
          env,
          Fn(Lambda(arg, b), envP, cont)) =>
      val lam                    = Lam(param, S.List(body :: Nil))
      def extend(e: Env)(x: Var) = if (x == arg) \/.right(Closure(lam, env)) else e(x)

      \/.right((b, extend(envP), cont))

    case (S.List(Nil), env, Ar(e, envP, cont)) => \/.right((e, envP, cont))
    case (S.List(Nil), env, Fn(Lambda(param, S.List(body :: Nil)), envP, cont)) =>
      \/.right((S.List(S.Atom("lambda") :: S.List(S.Atom(param) :: Nil) :: body :: Nil), envP, cont))

    case (S.List(x :: xs), env, Fn(lam, envP, cont)) =>
      \/.right((x, env, Fn(lam, envP, Ar(S.List(xs), env, cont))))

    case (S.List(x :: xs), env, cont) =>
      \/.right((x, env, Ar(S.List(xs), env, cont)))

    case (S.Atom(atom), env, cont) =>
      // TODO(lt) this is not exhaustive. stupid compiler
      env(atom) map {
        case Closure(Lambda(v, body), envP) =>
          (S.List(S.Atom("lambda") :: S.List(S.Atom(v) :: Nil) :: body :: Nil), envP, cont)
        case StringLiteral(s) => (S.String(s), env, cont)
        case NumLiteral(i)    => (S.Number(i), env, cont)
      }

    case (S.String(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x == arg) \/.right(StringLiteral(i)) else e(x)
      \/.right((b, extend(envP), cont))

    case (S.Number(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x == arg) \/.right((NumLiteral(i))) else e(x)
      \/.right((b, extend(envP), cont))

    case (S.Bool(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x == arg) \/.right((Bool(i))) else e(x)
      \/.right((b, extend(envP), cont))

    case (e, env, Ar(eP, envP, cont)) => \/.right((eP, envP, Ar(e, env, cont)))

    case _ => \/.left("stuck")
  }

  def isFinal(e: EvalState): Boolean = e._3 == Empty

  @annotation.tailrec
  def terminal(n: Int, l: List[(S.Expr, Cont)], eState: String \/ EvalState)
    : String \/ (EvalState, Integer, List[(S.Expr, Cont)]) = {
    eState match {
      case \/-(state) if isFinal(state) =>
        \/.right((state, n, (getExpr(state), getCont(state)) :: l))
      case \/-(state) =>
        terminal(n + 1, (getExpr(state), getCont(state)) :: l, step(state))
      case -\/(s) => \/.left(s + l.toString)
    }
  }

  def eval(e: S.Expr): String \/ (EvalState, Integer, List[(S.Expr, Cont)]) =
    terminal(0, Nil, step(inject(e)))

  def evaluate(e: S.Expr): String \/ DomainValue = {
    def interpret(env: Env, c: S.Expr): String \/ DomainValue = c match {
      case S.Bool(b) => \/.right(Bool(b))
      case S.Number(i) => \/.right(NumLiteral(i))
      case S.String(s) => \/.right(StringLiteral(s))
      case S.Atom(x) => env(x)
        //case S.List(xs) => VList()
      case S.Pair(a, b) => (interpret(env, a) |@| interpret(env, b))(VPair)
    }

    eval(e) match {
      case \/-((s, _, _)) => interpret(getEnv(s), getExpr(s))
    }
  }
}
