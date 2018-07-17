package com.github.skac112.funnodes

object FunNode {
  def split[F](node1: FunNode[F], node2: FunNode[F], splitFun: FunNode[F])(implicit fm: FluxManager[F]): FunNode[F] = new FunNode[F] {
    override def apply(flux: F): F = {
      val (flux1, flux2) = fm.split(flux, splitFun)
      fm.+(node1(flux1), node2(flux2))
    }
  }
}

trait FunNode[F] extends Function1[F, F]{

}
