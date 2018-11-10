package com.github.skac112.funnodes

import cats._
import cats.data._
import cats.implicits._
import SplitUntil._

object FunNode {
  case class QuickNode[M[_]: Monad, A, B](val quick_fun: A => M[B]) extends FunNode[M, A, B] {
    override def apply(arg: A) = quick_fun(arg)
  }

  /**
    * Sequence composition of two nodes. n2 is internal (applied first).
    * @param n1
    * @param n2
    * @tparam M
    * @tparam F
    * @return
    */
  def chain[M[_]: Monad, A, B, C](n1: FunNode[M, A, B], n2: FunNode[M, C, A]): FunNode[M, C, B] = QuickNode[M, C, B](n1.k.compose(n2.k).run)

  def chainUntil[M[_] : Monad, A, B](n: FunNode[M, A, B], endCond: M[B] => Boolean)(implicit ev: B <:< A) = ChainUntil(n, endCond)

  def branch[M[_] : Monad, A, B](n: FunNode[M, A, B], splitFun: SplitFun[A, B])(implicit ev: A <:< B) = Branch(n, splitFun)

  def branchUntil[M[_]: Monad, A](n: FunNode[M, A, A], splitFun: SplitFun[A, A], endCond: A => Boolean) =
    BranchUntil[M, A](n, splitFun, endCond)

  /**
    * Sequence composition of same node given number of times.
    * @param n
    * @param times
    * @tparam M
    * @tparam F
    * @return
    */
//  def chainOne[M[_]: Monad, A, B](n: FunNode[M, A, B], times: Int)(implicit ev: B <:< A): FunNode[M, A, B] =
//    (1 to times).foldLeft(n){(acc, _) => chain(acc, n)}


  /**
    * Creates node by splitting input flux between two nodes and merging their outputs.
    *
    * @param n1
    * @param n2
    * @param splitFun
    * @tparam M
    * @tparam F
    * @return
    */
  def split[M[_] : Monad, A, B](n1: FunNode[M, A, B], n2: FunNode[M, A, B], splitFun: SplitFun[A,B]) =
    Split(n1, n2, splitFun)

  def splitUntil[M[_] : Monad, A, B](n: FunNode[M, A, B], splitFun: SplitFun[A, B], endCond: A => Boolean) =
    QuickSplitUntil(n, splitFun, endCond)

  def mSplit[M[_]: Monad, A, B](n1: FunNode[M, A, B], n2: FunNode[M, A, B], mSplitFun: MSplitFun[M, A, B]): FunNode[M, A, B] = {
    val ff: A => M[B] = (flux: A) => {
      val (fx1, fx2, merge_fun) = mSplitFun(flux)
      merge_fun(n1(fx1), n2(fx2))
    }
    QuickNode[M, A, B](ff)
  }

  private def dummySplitFun[A, B]: SplitFun[A, B] = (flux: A) => (flux, flux, (flux1: B, flux2: B) => flux1)

  def split[M[_]: Monad, A, B](nodes: Seq[FunNode[M, A, B]], sfs: Seq[SplitFun[A, B]]): FunNode[M, A, B] =
    (nodes zip (sfs :+ dummySplitFun[A, B])) match {
      case Seq((node, splitFun)) => node
      case Seq((node, splitFun), tail @ _*) => split(node, split(tail map {arg: (FunNode[M, A, B], SplitFun[A, B]) => arg._1},
        tail map {arg: (FunNode[M, A, B], SplitFun[A, B]) => arg._2}), splitFun)
    }

  def splitOne[M[_]: Monad, A, B](n: FunNode[M, A, B], times: Int, splitFun: SplitFun[A, B]): FunNode[M, A, B] = times match {
    case 1 => split(n, n, splitFun)
    case _ => split(n, splitOne(n, times - 1, splitFun), splitFun)
  }

  /**
    * Creates node by splitting part of the input flux and directing it to other node, and then merging output of given
    * node with rest of the flux.
    * @param n
    * @param splitFun
    * @tparam M
    * @tparam F
    * @return
    */
//  def branch[M[_]: Monad, F](n: FunNode[M, F], splitFun: SplitFun[F]): FunNode[M, F] = {
//    val ff: F => M[F] = (flux: F) => {
//      val (fx1, fx2, merge_fun) = splitFun(flux)
//      for {
//        fxo1 <- n.k.run(fx1)
//      } yield (merge_fun(fxo1, fx2))
//    }
//
//    QuickNode[M, F](ff)
//  }

//  def branchOne[M[_]: Monad, F](n: FunNode[M, F], times: Int, splitFun: SplitFun[F]): FunNode[M, F] = ???
//  def branchUntil[M[_]: Monad, F](n: FunNode[M, F], splitFun: SplitFun[F], endCond: F => Boolean): FunNode[M, F] = ???

//  def parallel[]
}

abstract class FunNode[M[_] : Monad, A, B] extends Function1[A, M[B]] {
  import FunNode._
//  override def apply(flux: A): M[B] = k.run(flux)
  lazy val k: Kleisli[M, A, B] = Kleisli(this.apply)
//  def fun(arg: A): M[B]

  /**
    * Creates a node from this node by mapping output monad with given function.
    * @param f
    * @return
    */
  def postMap[C](f: B => C) = QuickNode((arg: A) => this.apply(arg).map(f))

  /**
    * Creates a node from this node by pre-composing internal function with given function - given function
    * is applied first.
    * @param f
    * @return
    */
  def preMap[C](f: C => A) =  QuickNode((arg: C) => this.apply(f(arg)))

//  implicit def toHomo[M, A, B](n: FunNode[M, A, B](implicit ev: A =:= B) =

  /**
    * Composition. other is applied first (is internal).
    * @param other
    * @return
    */
  def chain[C](other: FunNode[M, C, A]): FunNode[M, C, B] = FunNode.chain(this, other)
  def chainUntil(endCond: M[B] => Boolean)(implicit ev: B <:< A) = FunNode.chainUntil(this, endCond)
  def split(other: FunNode[M, A, B], splitFun: SplitFun[A,B]) = FunNode.split(this, other, splitFun)
  def splitUntil(splitFun: SplitFun[A, B], endCond: A => Boolean): FunNode[M, A, B] = FunNode.splitUntil(this, splitFun, endCond)
  def branch(splitFun: SplitFun[A,B])(implicit ev: A <:< B) = FunNode.branch(this, splitFun)
  def branchUntil(splitFun: SplitFun[A, A], endCond: A => Boolean)(implicit ev: A =:= B) =
    FunNode.branchUntil[M, A](this.asInstanceOf[FunNode[M, A, A]], splitFun, endCond)
}
