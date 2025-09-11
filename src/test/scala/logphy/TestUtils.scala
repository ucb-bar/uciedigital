package edu.berkeley.cs.ucie.digital
package logphy

import scala.collection.mutable
import scala.util.Random

object TestUtils {

  def createExpErrVecs(
      testVector: Seq[Seq[BigInt]],
      serializerRatio: Int,
  ): (Seq[mutable.Seq[BigInt]], Seq[Int]) = {
    val testErrSeq =
      testVector.map(vec => TestUtils.makeRandomErrors(vec, serializerRatio))
    val testVecs = testErrSeq.map(_._1)
    val errVecs =
      testErrSeq.map(_._2).reduce((x, y) => x.zip(y).map(f => f._1 + f._2))
    (testVecs, errVecs)
  }

  def oneToLanes(
      input: BigInt,
      numLanes: Int,
      serializerRatio: Int,
  ): Seq[BigInt] = {
    val result = mutable.Seq.fill(numLanes)(BigInt(0))
    var one = input
    for (i <- 0 until serializerRatio / 8) {
      for (j <- 0 until numLanes) {
        result(j) |= (one & 0xff) << (i * 8)
        one >>= 8
      }
    }
    result.toSeq
  }

  def lanesToOne(
      input: Seq[BigInt],
      numLanes: Int,
      serializerRatio: Int,
  ): BigInt = {
    var result = BigInt(0)
    val lanes = mutable.Seq(input: _*)
    for (i <- 0 until serializerRatio / 8) {
      for (j <- 0 until numLanes) {
        result |= (lanes(j) & 0xff) << ((i * numLanes + j) * 8)
        lanes(j) >>= 8
      }
    }
    result
  }

  def makeRandomErrors(
      input: Seq[BigInt],
      width: Int,
  ): (mutable.Seq[BigInt], Seq[Int]) = {
    val result = mutable.Seq(input: _*)
    val rand = new Random()
    val numErrors = Seq.fill(input.length)(rand.nextInt(width))
    for (i <- 0 until input.length) {
      val errorBits = mutable.Set[Int]()
      val numError = numErrors(i)
      while (errorBits.size < numError) {
        val bit = rand.nextInt(width)
        if (!errorBits.contains(bit)) {
          result(i) ^= BigInt(1) << bit
          errorBits += bit
        }
      }
    }
    (result, numErrors)
  }

  def makeRandomErrors(
      input: Seq[BigInt],
      numErrors: Seq[Int],
      width: Int,
  ): mutable.Seq[BigInt] = {
    val result = mutable.Seq(input: _*)
    val rand = new Random()
    for (i <- 0 until input.length) {
      val errorBits = mutable.Set[Int]()
      val numError = numErrors(i)
      while (errorBits.size < numError) {
        val bit = rand.nextInt(width)
        if (!errorBits.contains(bit)) {
          result(i) ^= BigInt(1) << bit
          errorBits += bit
        }
      }
    }
    result
  }
  def makeRandomErrors(
      input: Seq[BigInt],
      numErrors: Int,
      width: Int,
  ): mutable.Seq[BigInt] = {
    val result = mutable.Seq(input: _*)
    val errorBits = mutable.Set[(Int, Int)]()

    val rand = new Random()
    while (errorBits.size < numErrors) {
      val i = rand.nextInt(input.length)
      val bit = rand.nextInt(width)
      if (!errorBits.contains((i, bit))) {
        result(i) ^= BigInt(1) << bit
        errorBits += ((i, bit))
      }
    }
    result
  }
}
