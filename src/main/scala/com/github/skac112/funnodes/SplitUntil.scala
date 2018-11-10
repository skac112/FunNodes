package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._
import FunNode._

object SplitUntil {
  case class QuickSplitUntil[M[_] : Monad, A, B](override val n: FunNode[M, A, B], override val splitFun: SplitFun[A, B],
                                                 override val endCond: A => Boolean) extends SplitUntil[M, A, B]
}

/**
  * FunNode which splits flux while processing until a condition is met. Input flux is splitted in two parts. First
  * part is processed by given node. For the second part an end condition is checked. If condition holds, the second
  * part is also processed by a given node. If it doesn't, the second part is splitted and recurrently processed as
  * described above. In the end the two outputs are merged to give the result.
  * Performs at least one split and a given node is used at least two times (for both branches of splitted flux).
  * Splitting is done always on an input side and there is no chaining of processing.
  * @param n
  * @param splitFun
  * @param endCond
  * @param ev$1
  * @tparam M
  * @tparam A
  * @tparam B
  */
abstract class SplitUntil[M[_] : Monad, A, B] extends FunNode[M, A, B] {
  def n: FunNode[M, A, B]
  def splitFun: SplitFun[A, B]
  def endCond: A => Boolean

  override def apply(flux: A): M[B] = {
    val (fx1, fx2, merge_fun) = splitFun(flux)
    for {
      fxo1 <- n(fx1)
      fxo2 <- if (endCond(fx2)) n(fx2) else apply(fx2)
    } yield merge_fun(fxo1, fxo2)
  }
}
