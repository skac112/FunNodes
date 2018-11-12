package com.github.skac112.funnodes

import cats._
import cats.Monad
import cats.data.Writer
import scala.annotation.tailrec
import com.github.skac112.funnodes._
import com.github.skac112.funnodes.FunNode._
import SplitUntil._

object Example {
  type ExampleMonad[A] = cats.data.Writer[String, A]

  implicit val exampleMonadInstance: Monad[ExampleMonad] = new Monad[ExampleMonad] {
    def pure[A](a: A): ExampleMonad[A] = Writer("", a)
    def flatMap[A, B](fa: ExampleMonad[A])(f: A => ExampleMonad[B]): ExampleMonad[B] = {
      val new_writer = f(fa.value)
      Writer(fa.written ++ new_writer.written, new_writer.value)
    }
    override def map[A, B](fa: ExampleMonad[A])(f: A => B): ExampleMonad[B] = Writer(fa.written, f(fa.value))
    @tailrec
    def tailRecM[A, B](a: A)(f: A => ExampleMonad[Either[A, B]]): ExampleMonad[B] = {
      val new_w = f(a)
      val either_val = new_w.value
      either_val match {
        case Right(v) => Writer(new_w.written, v)
        case Left(v) => tailRecM(v)(f)
      }
    }
  }

  //   implicit val ensembleMonoidInstance: Monoid[Ensemble] = new Monoid[Ensemble] {
  //     override def combine(ens1: Ensemble, ens2: Ensemble) = ens1 ++ ens2
  //     override def empty = Seq.empty[PosGraphic]
  //   }
}

import Example._

object Main extends App {
  val n = QuickNode[ExampleMonad, Double, Double]((a: Double) => Writer(s"wynik = ${a*2} ", a*2))
  val splitFun = (a: Double) => (.6*a, .4*a, ((a: Double, b: Double) => a + b))
  val endCond = (a: Double) => a < 1
  val s1 = QuickSplitUntil(n, splitFun, endCond)
  val s2 = n.splitUntil(splitFun, endCond)
  val s3 = SplitUntil(n, splitFun, endCond)
  val res1 = s1(10)
  val res2 = s2(10)
  val res3 = s3(10)
  println(res1)
  println(res2)
  println(res3)
}
