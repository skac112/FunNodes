package com.github.skac112

package object funnodes {
  /**
    * A triple which (for some set input flux) gives a two subfluxes and a merge function which enables merging
    * fluxes after modifying them in a way according to previous splitting operation.
    * @tparam F
    */
  type Split[F] = (F, F, MergeFun[F])

  /**
    * Function which returns a split for given input flux.
    * @tparam F
    */
  type SplitFun[F] = F => Split[F]

  /**
    * MergeFun is a function which gives a flux for given subfluxes.
    * @tparam F
    */
  type MergeFun[F] = (F, F) => F
}
