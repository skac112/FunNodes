package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._
import com.github.skac112.funnodes.FunNode.QuickNode

object BranchUntil {
  case class QuickBranchUntil[M[_] : Monad, A](override val n: FunNode[M, A, A],
                                                  aSplitFun: SplitFun[A, A],
                                                  aEndCond: A => Boolean)
  extends BranchUntil[M, A] {
    override def splitFun(flux: A) = aSplitFun(flux)
    override def endCond(flux: A) = aEndCond(flux)
  }

  def apply[M[_] : Monad, A](n: FunNode[M, A, A], splitFun: SplitFun[A, A], endCond: A => Boolean) =
    QuickBranchUntil(n, splitFun, endCond)
}

/**
  * @param n
  * @param splitFun
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
abstract class BranchUntil[M[_] : Monad, A] extends FunNode[M, A, A] {
  def n: FunNode[M, A, A]
  def splitFun(flux: A): SplitMerge[A, A]
  def endCond(flux: A): Boolean

  override def apply(flux: A): M[A] = {
    val (fx1, fx2, merge_fun) = splitFun(flux)
    val m = n(fx1)
    for {
      fxo1 <- m
      fxo11 <- if (endCond(fxo1)) m else apply(fxo1)
    } yield merge_fun(fxo11, fx2)
  }
}