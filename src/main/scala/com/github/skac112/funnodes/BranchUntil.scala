package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._
import com.github.skac112.funnodes.FunNode.QuickNode

/**
  * @param n
  * @param splitFun
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
case class BranchUntil[M[_] : Monad, A](n: FunNode[M, A, A], splitFun: SplitFun[A, A], endCond: A => Boolean)
  extends FunNode[M, A, A] {
  override def apply(flux: A): M[A] = {
    val (fx1, fx2, merge_fun) = splitFun(flux)
    val m = n(fx1)
    for {
      fxo1 <- m
      fxo11 <- if (endCond(fxo1)) m else apply(fxo1)
    } yield merge_fun(fxo11, fx2)
  }
}