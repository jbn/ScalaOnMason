/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.reflect.BeanProperty

import ec.util.MersenneTwisterFast

import com.pathdependent.mason.ext.PBM

/**
 * A resource is something that agents want to collect (e.g. sugar, spice.)
 */
class Resource(
  @BeanProperty var level: Double, 
  @BeanProperty var capacity: Double
) extends Serializable {

  /** Grow back one unit per step. */
  def unitGrowback() { level = (level + 1.0) min capacity }
  
  /** Growback delta units per step. */   
  def constantGrowback(delta: Double) {
    level = (level + delta) min capacity
  }
  
  /** Immediately growback to full capacity. */
  def capacityGrowback() { level = capacity }
  
  /**
   * The resource will grow back at the current level * (1 + growbackRate). 
   * Since an exhausted resource will be 0, there must be a level following
   * exhaustion that is constant. That is reflected in refractoryLevel.
   */ 
  def exponentialGrowback(refractoryLevel: Double, growbackRate: Double) {
    level = 
      if(level == 0) refractoryLevel min capacity
      else (level * (1.0 + growbackRate)) min capacity
  }
  
  /**
   * Allows a resource to growback at different (albeit constant) rates 
   * based on the "season." 
   *
   * Epstein and Axtell used 1/8 and 1 for winter and summer, respectively.
   *
   * @see Growing Artificial Societies, p.44.
   */
  def simpleSeasonalGrowback(
    isNorth: Boolean,
    timeInSteps: Long,
    durationOfSeasonInSteps: Long,
    summerGrowbackRate: Double,
    winterGrowbackRate: Double
  ) {
    val firstSeason = timeInSteps % 
      (2.0 * durationOfSeasonInSteps) / durationOfSeasonInSteps < 1
      
    val growbackRate = 
      if(isNorth) { 
        if(firstSeason) summerGrowbackRate else winterGrowbackRate
      } else {
        if(firstSeason) winterGrowbackRate else summerGrowbackRate
      }
      
    constantGrowback(growbackRate)
  }
}

