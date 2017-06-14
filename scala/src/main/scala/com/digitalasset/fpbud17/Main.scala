package com.digitalasset.fpbud17

import scalaz.syntax.monad._
import scalaz.syntax.std.option._

object Main extends App {
  private val (status, out, res) =
    (args.headOption \/> ("No arguments") >>= Parser.parseExpr >>= Eval.evaluate)
      .leftMap("error: " + _)
      .fold((1, Console.err, _), (0, Console.out, _))

  out.println(res)
  //sys.exit(status)
}
