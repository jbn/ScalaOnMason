/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.ext

import scala.collection.mutable.BitSet

import ec.util.MersenneTwisterFast

/**
 * The BinaryString class thinly wraps a mutable BitSet. 
 * BinaryStrings have a fixed, known length, whereas BitSet's are expandable.
 * BitSet was wrapped for simple bit packing into 64-bit words. 
 */
case class BinaryString(val length: Int) {
  val onBits = new BitSet()

  /**
   * @return the length of the string, not the number of set bits.
   */
  def size(): Int = length
  
  /**
   * @return the number of set bits (called cardinality in Java's BitSet)
   */
  def numberOfTrueBits(): Int = onBits.size

  /**
   * @return the fraction of bits set to true
   */
  def fractionOfTrueBits(): Double = numberOfTrueBits / length.toDouble
  
  /**
   * @return a deep-copy of this BinaryString.
   */
  def duplicate(): BinaryString = {
    val duplicated = new BinaryString(length)
    for(i <- 0 until length if this(i)){
      duplicated(i) = true
    }
    duplicated
  }
  
  /**
   * @return true if the bit is set
   */
  def apply(i: Int) = onBits(i)
  
  /**
   * Sets the bit at index i to value
   */
  def update(i: Int, value: Boolean) { onBits(i) = value }
  
  /**
   * A Hamming Distance calculation requires strings of equal length. 
   *
   * @return Hamming distance between this string and other
   * @see http://en.wikipedia.org/wiki/Hamming_distance
   */
  def hammingDistance(other: BinaryString): Int = {
    assume(other.length == length)
    
    var deviations = 0
    for(i <- 0 until length if onBits(i) != other(i)) deviations += 1    
    deviations
  }
  
  override def toString(): String = {
    (0 until length).map(i => if(onBits(i)) '1' else '0').mkString
  }
}

object BinaryString {
  def generateRandom(rng: MersenneTwisterFast, length: Int): BinaryString = {
    val generated = new BinaryString(length)
    
    (0 until length).foreach{ i => if(rng.nextBoolean) generated(i) = true }
    
    generated
  }
}
