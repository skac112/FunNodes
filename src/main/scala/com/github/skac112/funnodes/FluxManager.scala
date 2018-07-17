package com.github.skac112.funnodes

/**
  * Handles basic operations on flux which are necessary to composing funnodes.
  * @tparam F
  */
trait FluxManager[F] {
  def +(flux1: F, flux2: F): F
  def -(flux1: F, flux2: F): F

  def split(flux: F, splitFun: FunNode[F]): (F, F) = {
    val f1 = splitFun(flux)
    (f1, this.-(flux, f1))
  }
}
