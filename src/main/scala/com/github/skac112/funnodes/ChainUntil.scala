package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._

object ChainUntil {
  case class QuickChainUntil[M[_]: Monad, A, B](override val n: FunNode[M, A, B],
                                                  aEndCond: B => Boolean)(implicit ev: B <:< A)
    extends ChainUntil[M, A, B] {
    override def endCond(flux: B) = aEndCond(flux)
  }

  def apply[M[_]: Monad, A, B](n: FunNode[M, A, B], endCond: B => Boolean)(implicit ev: B <:< A) = QuickChainUntil(n, endCond)
}

abstract class ChainUntil[M[_]: Monad, A, B](implicit ev: B <:< A) extends FunNode[M, A, B] {
  def n: FunNode[M, A, B]
  def endCond(flux: B): Boolean

  override def apply(flux: A): M[B] = {
    val m = n(flux)
    for {
      f <- m
      f2 <- if (endCond(f)) m else apply(f)
    } yield f2
  }
}
