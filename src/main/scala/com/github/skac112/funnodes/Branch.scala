package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._

/**
  * Nodes which splits flux in two part and procedes one of them by a given node. Other part is unchanged. Both
  * parts are them merged.
  * @param n
  * @param splitFun
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
case class Branch[M[_] : Monad, A, B](n: FunNode[M, A, B], splitFun: SplitFun[A,B])(implicit ev: A <:< B) extends FunNode[M, A, B] {
  override def apply(flux: A): M[B] = {
    val (fx1, fx2, merge_fun) = splitFun(flux)
    for {
      fxo1 <- n(fx1)
    } yield merge_fun(fxo1, fx2)
  }
}
