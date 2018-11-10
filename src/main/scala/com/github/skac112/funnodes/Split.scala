package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._

case class Split[M[_] : Monad, A, B](n1: FunNode[M, A, B], n2: FunNode[M, A, B], splitFun: SplitFun[A,B]) extends FunNode[M, A, B] {
  override def apply(flux: A): M[B] = {
    val (fx1, fx2, merge_fun) = splitFun (flux)
    for {
      fxo1 <- n1(fx1)
      fxo2 <- n2(fx2)
    } yield merge_fun(fxo1, fxo2)
  }
}
