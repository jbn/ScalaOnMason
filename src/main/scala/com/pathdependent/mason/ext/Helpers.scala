/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.ext

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

import sim.engine.{SimState, Steppable}
import ec.util.MersenneTwisterFast

object Helpers {
  /**
   * Allows you to create a steppable class, DSL-style.
   *
   * Beware your closures.
   */
  def makeSteppable[T](f: (T) => Unit) = new Steppable {
    def step(state: SimState) = f(state.asInstanceOf[T])
  }
  
  /**
   * Shuffles a collection <em>out of place</em>.
   *
   * @param  coll    the [[scala.collection.TraversableOnce]] to shuffle
   *
   * @return a new collection composed of the original collection ordered 
   *         randomly.
   *
   * This was taken directly from the scala standard library Random.scala.
   *
   * I tried extending MASON's collections (e.g. Bag) with types, but it was
   * to difficult, messy, and seemed error prone. However, most of the time,
   * I want to use Scala collections anyway, but require access to a shuffle
   * method that uses a MersenneTwisterFast instead of a Java-based Random. 
   *
   * @todo optimize the shufflers based on the collection type. 
   * 
   */
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T], rng: MersenneTwisterFast)
      (implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs
        
    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }
    
    for (n <- buf.length to 2 by -1) {
      val k = rng.nextInt(n)
      swap(n - 1, k)
    }
    
    bf(xs) ++= buf result
  }
  
  /**
   * From a Scala perspective, this is nicer...I think
   */
  def doSimulationLoop[T](args: Array[String])
      (implicit manifest: scala.reflect.Manifest[T]) {
    sim.engine.SimState.doLoop(manifest.erasure, args)
  }
  
  def sum(bag: sim.util.IntBag): Int = {
    var accumulator = 0
    (0 until bag.numObjs).foreach{ accumulator += bag.get(_) }
    accumulator
  }
  
  def sum(bag: sim.util.DoubleBag): Double = {
    var accumulator = 0.0
    (0 until bag.numObjs).foreach{ accumulator += bag.get(_) }
    accumulator
  }
}
