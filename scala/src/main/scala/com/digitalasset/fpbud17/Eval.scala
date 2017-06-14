package com.digitalasset.fpbud17

import com.digitalasset.fpbud17.{Scheme => S}

import Function.const

import scalaz.{@@, Tag, -\/, \/, \/-, Equal}
import scalaz.syntax.applicative._
import scalaz.std.string._
import scalaz.syntax.equal._

object Eval {

  type Var = String

  sealed trait LambdaTag
  type Lambda = (Var, S.Expr) @@ LambdaTag
  val Lambda: Tag.TagOf[LambdaTag]   = Tag.of[LambdaTag]
  def Lam(v: Var, e: S.Expr): Lambda = Lambda((v, e))

  type Env = Var => String \/ DomainValue

  sealed trait DomainValue
  final case class Closure(lambda: Lambda, env: Env)         extends DomainValue
  final case class StringLiteral(value: String)              extends DomainValue
  final case class NumLiteral(value: BigInt)                 extends DomainValue
  final case class Bool(value: Boolean)                      extends DomainValue
  final case class VList(values: List[DomainValue])          extends DomainValue
  final case class VPair(fst: DomainValue, snd: DomainValue) extends DomainValue

  object Cont {
    implicit val contEqual: Equal[Cont] = Equal.equalA[Cont]
  }
  sealed trait Cont
  final case object Empty                         extends Cont
  final case class Ar(c: S.Expr, e: Env, k: Cont) extends Cont
  final case class Fn(l: Lambda, e: Env, k: Cont) extends Cont

  // CEK Machine state
  // C := Expression, E := Env, K := Cont
  type EvalState = (S.Expr, Env, Cont)
  def Right(c: S.Expr, e: Env, k: Cont): String \/ EvalState = \/.right((c, e, k))
  def getExpr(s: EvalState): S.Expr                          = s._1
  def getEnv(s: EvalState): Env                              = s._2
  def getCont(s: EvalState): Cont                            = s._3

  def inject(e: S.Expr): EvalState = (e, const(\/.left(s"not bound value: $e")), Empty)
  //def inject(e: S.Expr): EvalState = (e, (v => sys.error(s"not bound value: $e")), Empty)

  def step(e: EvalState): String \/ EvalState = e match {
    case (S.List(List(S.Atom("lambda"), S.List(List(S.Atom(param))), body)),
          env,
          Ar(e, envP, cont)) =>
      val lam = Lam(param, S.List(body :: Nil))
      \/.right((e, envP, Fn(lam, env, cont)))

    case (S.List(List(S.Atom("lambda"), S.List(List(S.Atom(param))), body)),
          env,
          Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) =
        if (x === arg) \/.right(Closure(Lam(param, S.list(body)), env))
        else e(x)

      \/.right((b, extend(envP), cont))

    case (S.List(Nil), env, Ar(e, envP, cont)) =>
      \/.right((e, envP, cont))

    case (S.List(Nil), env, Fn(Lambda(param, S.List(List(body))), envP, cont)) =>
      \/.right((S.list(S.Atom("lambda"), S.list(S.Atom(param)), body), envP, cont))

    case (S.List(x :: xs), env, Fn(lam, envP, cont)) =>
      \/.right((x, env, Fn(lam, envP, Ar(S.List(xs), env, cont))))

    case (S.List(x :: xs), env, cont) =>
      \/.right((x, env, Ar(S.List(xs), env, cont)))

    case (S.Atom(atom), env, cont) =>
      // TODO(lt) this is not exhaustive. stupid compiler
      env(atom) map {
        case Closure(Lambda(v, body), envP) =>
          (S.list(S.Atom("lambda"), S.list(S.Atom(v)), body), envP, cont)
        case StringLiteral(s) => (S.String(s), env, cont)
        case NumLiteral(i)    => (S.Number(i), env, cont)
      }

    case (S.String(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x === arg) \/.right(StringLiteral(i)) else e(x)
      \/.right((b, extend(envP), cont))

    case (S.Number(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x === arg) \/.right((NumLiteral(i))) else e(x)
      \/.right((b, extend(envP), cont))

    case (S.Bool(i), env, Fn(Lambda(arg, b), envP, cont)) =>
      def extend(e: Env)(x: Var) = if (x === arg) \/.right((Bool(i))) else e(x)
      \/.right((b, extend(envP), cont))

    case (e, env, Ar(eP, envP, cont)) =>
      \/.right((eP, envP, Ar(e, env, cont)))

    case _ => \/.left("stuck")
  }

  def isFinal(e: EvalState): Boolean = e._3 === Empty

  @annotation.tailrec
  def terminal(
      n: Int,
      l: List[(S.Expr, Cont)],
      eState: String \/ EvalState): String \/ (EvalState, Integer, List[(S.Expr, Cont)]) = {
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
    import scalaz.std.list._
    import scalaz.syntax.traverse._

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def interpret(env: Env): S.Expr => String \/ DomainValue = {
      case S.Bool(b)    => \/.right(Bool(b))
      case S.Number(i)  => \/.right(NumLiteral(i))
      case S.String(s)  => \/.right(StringLiteral(s))
      case S.Atom(x)    => env(x)
      case S.List(xs)   => xs.traverseU(interpret(env)).map(VList)
      case S.Pair(a, b) => (interpret(env)(a) |@| interpret(env)(b))(VPair)
    }

    eval(e) match {
      case \/-((s, _, _)) => interpret(getEnv(s))(getExpr(s))
      case -\/(e)         => \/.left(e)
    }
  }
}
