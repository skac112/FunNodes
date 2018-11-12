package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._

object Split {
  case class QuickSplit[M[_] : Monad, A, B](override val n1: FunNode[M, A, B],
                                            override val n2: FunNode[M, A, B],
                                            aSplitFun: SplitFun[A,B])
    extends Split[M, A, B] {
    override def splitFun(flux: A) = aSplitFun(flux)
  }

  def apply[M[_] : Monad, A, B](n1: FunNode[M, A, B], n2: FunNode[M, A, B], splitFun: SplitFun[A, B]) =
    QuickSplit(n1, n2, splitFun)
}

abstract class Split[M[_] : Monad, A, B] extends FunNode[M, A, B] {
  def n1: FunNode[M, A, B]
  def n2: FunNode[M, A, B]
  def splitFun(flux: A): SplitMerge[A, B]

  override def apply(flux: A): M[B] = {
    val (fx1, fx2, merge_fun) = splitFun (flux)
    for {
      fxo1 <- n1(fx1)
      fxo2 <- n2(fx2)
    } yield merge_fun(fxo1, fxo2)
  }
}
