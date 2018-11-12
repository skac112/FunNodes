package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._
import FunNode._

object SplitWhile {
  case class QuickSplitWhile[M[_]: Monad, A, B](override val n: FunNode[M, A, B],
                                 aSplitFun: SplitFun[A, B],
                                 aWhileCond: A => Boolean)
  extends SplitWhile[M, A, B] {
    override def splitFun(flux: A) = aSplitFun(flux)
    override def whileCond(flux: A) = aWhileCond(flux)
  }

  def apply[M[_]: Monad, A, B](n: FunNode[M, A, B], splitFun: SplitFun[A, B], whileCond: A => Boolean) =
    QuickSplitWhile(n, splitFun, whileCond)
}

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
abstract class SplitWhile[M[_] : Monad, A, B] extends FunNode[M, A, B] {
  def n: FunNode[M, A, B]
  def splitFun(flux: A): SplitMerge[A, B]
  def whileCond(flux: A): Boolean

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
