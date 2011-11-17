/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.reflect.BeanProperty

object Resource {
  type GrowbackRule = (Resource) => Double
  
  def CapacityLimited(resource: Resource, growback: GrowbackRule): Double = {
    resource.capacity min growback(resource)
  }
  
  /** The default next level of GAS assumes unit growback. */
  val UnitGrowback: GrowbackRule = (resource) => resource.level + 1 
  
  /** The resource will grow back to its full capacity on every step. */
  val CapacityGrowback: GrowbackRule = (resource) => resource.capacity

  /** The resource will grow back growbackRate units per step. */   
  def makeConstantGrowbackRule(growbackRate: Double): GrowbackRule = {
    (resource: Resource) => resource.level + growbackRate
  }
  
  /**
   * The resource will grow back at the current level * (1 + growbackRate). 
   * Since an exhausted resource will be 0, there must be a level following
   * exhaustion that is constant. That is reflected in refractoryLevel.
   */ 
  def makeExponentialGrowback(refractoryLevel: Double, 
                              growbackRate: Double): GrowbackRule = {
    (resource: Resource) => { 
      if(resource.level == 0){
        refractoryLevel
      }else{
        (resource.level * (1.0 + growbackRate))
      }
    } 
  }
}

class Resource(
  @BeanProperty var level: Double, 
  @BeanProperty var capacity: Double
) extends Serializable
