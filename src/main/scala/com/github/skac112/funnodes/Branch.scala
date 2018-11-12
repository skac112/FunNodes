package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._

object Branch {
  case class QuickBranch[M[_] : Monad, A, B](override val n: FunNode[M, A, B],
                                             aSplitFun: SplitFun[A, B])
                                            (implicit ev: A <:< B)
  extends Branch[M, A, B] {
    override def splitFun(flux: A) = aSplitFun(flux)
  }

  def apply[M[_] : Monad, A, B](n: FunNode[M, A, B], splitFun: SplitFun[A, B])(implicit ev: A <:< B) =
    QuickBranch(n, splitFun)
}

/**
  * Nodes which splits flux in two part and procedes one of them by a given node. Other part is unchanged. Both
  * parts are then merged.
  * @param n
  * @param splitFun
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
abstract class Branch[M[_] : Monad, A, B](implicit ev: A <:< B) extends FunNode[M, A, B] {

  def n: FunNode[M, A, B]
  def splitFun(flux: A): SplitMerge[A, B]

  override def apply(flux: A): M[B] = {
    val (fx1, fx2, merge_fun) = splitFun(flux)
    for {
      fxo1 <- n(fx1)
    } yield merge_fun(fxo1, fx2)
  }
}
