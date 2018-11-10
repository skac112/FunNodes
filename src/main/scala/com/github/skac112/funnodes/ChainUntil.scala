package com.github.skac112.funnodes

import cats.Monad
import cats.data._
import cats.implicits._

case class ChainUntil[M[_] : Monad, A, B](n: FunNode[M, A, B], endCond: M[B] => Boolean)(implicit ev: B <:< A) extends FunNode[M, A, B] {
  override def apply(flux: A): M[B] = {
    val m = n(flux)
    endCond(m) match {
      case false => for {
        f <- m
        f2 <- apply(f)
      } yield f2
      case true => m
    }
  }

//
//    override def apply(flux: A): M[B] = {
//    def chainUntilInt(m: M[B]): M[B] = endCond(m) match {
//      case false => for {
//        f <- m
//        f2 <- chainUntilInt(n(f))
//      } yield f2
//      case true => m
//    }
//
//    val m = n(flux)
//    chainUntilInt(m)
//  }
}

