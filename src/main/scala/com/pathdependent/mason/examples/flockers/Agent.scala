/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.examples.flockers

import scala.reflect.{BeanProperty, BooleanBeanProperty}
import scala.math.{atan2, cos, pow, sin, sqrt}
import scala.collection.JavaConversions._

import sim.engine.{SimState, Steppable}
import sim.field.continuous.Continuous2D
import sim.portrayal.Orientable2D
import sim.util.{Bag, Double2D, MutableDouble2D}
import ec.util.MersenneTwisterFast

/** 
 * The flocker is the "agent" of this model. 
 *
 * By using Scala's BeanProperty and BooleanBeanProperty annotations, 
 * variables and values can be made visible by MASON's inspectors.
 *
 * I don't like reasoning about state. I like to think of agent as objects for
 * state transformations. If there weren't performance issues, I'd like to
 * them all to be immutable. Since that's not possible, I try to minimize the
 * number of variables.
 *
 * @param location is the location of the object. I use the convention of
 *       prefixing the variable name with an underscore to denote variables
 *       that exist for convience or caching. In this case, the flockers
 *       location is determined by its position in the underlying 
 *       Continuous2D, not this field. I'm hoping to find a way to extract
 *       this pattern to a library feature (perhaps an annotation.)
 *
 * @param orientation2D The orientation in radians. I've removed the  
 *                      lastDirection property. The orientation is more 
 *                      ...central...to the model. 
 *                      
 *                      orientation2D(): Double is required by 
 *                      the Oriented2D interface.
 *
 *                      setOrientation2D(Double) is required by the
 *                      Orientable2D interface.
 * 
 *
 * @note Remember that case classes generate hash codes based on class
 *       parameters. If there is a variable parameter, the hashing will be
 *       inconsistent for storage in hash maps. This can lead to some really
 *       ugly bugs.
 *
 *       Easiest fix: don't use case classes for agents.
 */
class Flocker(
  var location: Double2D,
  @BooleanBeanProperty var dead: Boolean,
  var orientation2D: Double
) extends Steppable with Orientable2D {
  var momentum = new Double2D(cos(orientation2D), sin(orientation2D))
  /**
   * Gets the nearby neighbors assuming a terroidal landscape.
   */ 
  def neighbors(landscape: Continuous2D, simulation: Flockers): Bag = {
    landscape.getObjectsExactlyWithinDistance(
      location, simulation.params.neighborhoodRadius, true
    )
  }

  /**
   * This method is required by Oriented2D which is inhereted by Orientable2D.
   */  
  def updateOrientation2D(x: Double, y: Double) {
    orientation2D = if(x == 0 && y == 0) 0.0 else atan2(y, x)
    momentum = new Double2D(x, y)
  }
  
  def getOrientation2D = orientation2D
  
  def setOrientation2D(value: Double) {
    val x = cos(value)
    val y = sin(value)
    updateOrientation2D(x, y)
  }
  
  /** 
   * We're in a functional langauge. We can abstract control patterns!
   */
  def flockCalculation(neighbors: List[Flocker])
      (f: (Flocker) => Double2D): Double2D = {
    if(neighbors.isEmpty) { 
      return new Double2D(0.0, 0.0) 
    } else {
      val accumulator = new MutableDouble2D(0.0, 0.0)

      neighbors.foreach { neighbor => accumulator.addIn(f(neighbor)) }
      accumulator.multiplyIn(1.0 / neighbors.length)
    
      new Double2D(accumulator.x, accumulator.y)
    }
  }
  
  /** 
   * As per Sean Luke's documentation, Consistency (coherence) is:
   *   
   * <blockquote>
   *   A vector in the direction other flockers are going. 
   *   This is computed as the sum, over all live neighbors, 
   *   of the momentum vector of each neighbor.
   * </blockquote>
   */
  def consistencyCalculation(neighbors: List[Flocker]): Double2D = {
    flockCalculation(neighbors)(_.momentum)
  }
  
  /** 
   * As per Sean Luke's documentation, cohesion is:
   *
   * <blockquote>
   *    A vector towards the "center of mass" of nearby flockers. This is
   *    computed as the sum, over all live neighbors, of a vector towards 
   *    the neighbor.
   * </blockquote>
   */
  def cohesionCalculation(neighbors: List[Flocker], environment: Continuous2D): Double2D = {
    flockCalculation(neighbors) {
      (other) => new Double2D(
        environment.tdx(location.x, other.location.x), 
        environment.tdy(location.y, other.location.y)
      )
    }.multiply(-1.0/10.0)
  }
  
  def avoidanceCalculation(neighbors: List[Flocker], environment: Continuous2D): Double2D = {
    flockCalculation(neighbors) {
      (other) => 
        val xDistance = environment.tdx(location.x, other.location.x)
        val yDistance = environment.tdy(location.y, other.location.y)
        val divisor = pow(pow(xDistance, 2.0) + pow(yDistance, 2.0), 2.0) + 1
        new Double2D(xDistance / divisor, yDistance / divisor)
    }.multiply(400.0)
  }
  
  def generateRandomness(rng: MersenneTwisterFast): Double2D = {
    val x = rng.nextDouble * 2.0 - 1.0
    val y = rng.nextDouble * 2.0 - 1.0
    val l = sqrt(pow(x, 2.0) + pow(y, 2.0))
    
    new Double2D(0.05 * x / l, 0.5 * y / l)
  }
  
  def step(state: SimState) {
    if(!dead) {
      move(state.asInstanceOf[Flockers])
    }
  }
  
  def move(simulation: Flockers) {
    import simulation.params.{
      cohesionWeight, avoidanceWeight, consistencyWeight,
      randomnessWeight, momentumWeight, jumpDistance, neighborhoodRadius
    }
        
    // Collect the neighbors.
    //   Sean Luke's code included the the this object in the neigbhors,
    //   except for avoidance. 
    val allNeighbors = simulation.flock.getObjectsExactlyWithinDistance(
      location, neighborhoodRadius, true
    ).map(_.asInstanceOf[Flocker]).filterNot(_ eq this).toList
 
    val livingNeighbors = allNeighbors.filterNot(_.dead)
      
    val randomness  = generateRandomness(simulation.random)
    val avoidance   = avoidanceCalculation(allNeighbors, simulation.flock)
    val cohesion    = cohesionCalculation(livingNeighbors, simulation.flock)
    val consistency = consistencyCalculation(livingNeighbors)
    
    // val momentum = this is being calculated twice, since I don't store it.

    var xDistance = (
      cohesionWeight * cohesion.x +
      avoidanceWeight * avoidance.x +
      consistencyWeight * consistency.x +
      randomnessWeight * randomness.x +
      momentumWeight * momentum.x
    )
      
    var yDistance = (
      cohesionWeight * cohesion.y +
      avoidanceWeight * avoidance.y +
      consistencyWeight * consistency.y +
      randomnessWeight * randomness.y +
      momentumWeight * momentum.y
    ) 
      
    // Normalize to the maximum jump per step.
    val distance = sqrt(pow(xDistance, 2.0) + pow(yDistance, 2.0))
    if(distance > 0.0) {
      val divisor = distance / jumpDistance
      xDistance /= divisor
      yDistance /= divisor
    }
      
    updateOrientation2D(xDistance, yDistance)

    location = new Double2D(
      simulation.flock.stx(location.x + xDistance),
      simulation.flock.stx(location.y + yDistance)
    )
      
    simulation.flock.setObjectLocation(this, location)
  }
}






