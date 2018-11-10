package com.github.skac112

package object funnodes {
//  type NodeF[F] = F => F

//  type NodeM[M[_]] = Monad[M[_]]

  /**
    * A triple which (for some set input flux) gives a two subfluxes and a merge function which enables merging
    * fluxes after modifying them in a way according to previous splitting operation.
    * @tparam A
    */
  type SplitMerge[A, B] = (A, A, MergeFun[B])

  /**
    * Function which returns a split for given input flux.
    * @tparam F
    */
  type SplitFun[A, B] = A => SplitMerge[A, B]

  /**
    * MergeFun is a function which gives a flux for given subfluxes.
    * @tparam F
    */
  type MergeFun[A] = (A, A) => A

  type MSplit[M[_], A, B] = (A, A, MMergeFun[M, B])

  type MSplitFun[M[_], A, B] = A => MSplit[M, A, B]

  type MMergeFun[M[_], B] = (M[B], M[B]) => M[B]
}