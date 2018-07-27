package com.github.skac112.funnodes

object FunNode {

  /**
    * Creates node by splitting input flux between two nodes and merging outputs.
    * @param node1
    * @param node2
    * @param splitFun
    * @param fm
    * @tparam F
    * @return
    */
  def nodeSplit[F](node1: FunNode[F], node2: FunNode[F], sf: SplitFun[F]): FunNode[F] = new FunNode[F] {
    override def apply(flux: F): F = {
      val split = sf(flux)
      val (flux1, flux2, mf) = (split._1, split._2, split._3)
      mf(node1(flux1), node2(flux2))
    }
  }

  /**
    * Creates node by splitting input flux between many nodes and merging outputs.
    * @param nodes
    * @param sfs
    * @tparam F
    * @return
    */
  def nodeSplit[F](nodes: List[FunNode[F]], sfs: Seq[SplitFun[F]]): FunNode[F] = nodes match {
    case n1 :: n2 :: Nil => nodeSplit(n1, n2, sfs(0))
    case n1 :: _ :: _ => nodeSplit(n1, nodeSplit(nodes.tail, sfs.tail), sfs(0))
  }

  /**
    * Creates node by sending an input flux in parallel to two component nodes and merging output.
    * @param node1
    * @param node2
    * @param mf
    * @tparam F
    * @return
    */
  def nodeParallel[F](node1: FunNode[F], node2: FunNode[F], mf: MergeFun[F]): FunNode[F] = new FunNode[F] {
    override def apply(flux: F): F = mf(node1(flux), node2(flux))
  }

  /**
    * Identity node - just returns input flux.
    * @tparam F
    * @return
    */
  def id[F] = new FunNode[F] {
    override def apply(flux: F) = flux
  }

  /**
    * Continues using n node as long as guard is satisfied (possibly zero times - then returns input flux).
    * @param n
    * @param guard
    * @tparam F
    * @return
    */
  def loop[F](n: FunNode[F], guard: F => Boolean) = new FunNode[F] {
    override def apply(flux: F): F = {
      guard(flux) match {
        case true => apply(n(flux))
        case _ => flux
      }
    }
  }
}

trait FunNode[F] extends Function1[F, F] {
  import FunNode._
  def +(other: FunNode[F]) = new FunNode[F] {
    override def apply(flux: F) = FunNode.this(other(flux))
  }

  def *(count: Int) = ???

  def ||(other: FunNode[F], sf: SplitFun[F]) = nodeSplit(this, other, sf)

  /**
    * Splitting between this node and id node - flux not flowing by this node is just non-modified.
    * @param sf
    * @return
    */
  def |-(sf: SplitFun[F]) = nodeSplit(this, id, sf)

  def |||(other: FunNode[F], mf: MergeFun[F]) = nodeParallel[F](this, other, mf)

  def loop(guard: F => Boolean) = FunNode.loop[F](this, guard)
}
