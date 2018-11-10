package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._
import FunNode._

/**
  * FunNode which splits flux while processing as long as a condition is met. Uses given funnode to process splitted flux.
  * Performs zero or more splits and a given node is used at least one (for both branches of splitted flux).
  * @param n
  * @param splitFun
  * @param whileCond
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
class SplitWhile[M[_] : Monad, A, B](val n: FunNode[M, A, B], val splitFun: SplitFun[A, B], val whileCond: A => Boolean) extends FunNode[M, A, B] {
  override def apply(flux: A): M[B] = {
    if (!whileCond(flux)) {
      n(flux)
    }
    else {
      val (fx1, fx2, merge_fun) = splitFun(flux)
      for {
        fxo1 <- n(fx1)
        fxo2 <- apply(fx2)
      } yield merge_fun(fxo1, fxo2)
    }
  }
}
