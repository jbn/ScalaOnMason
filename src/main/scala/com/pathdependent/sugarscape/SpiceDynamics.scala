/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.reflect.{BeanProperty}

import sim.engine.{Steppable, SimState}
import sim.field.grid.ObjectGrid2D
import sim.portrayal.grid.ObjectGridPortrayal2D
import sim.portrayal.simple.OvalPortrayal2D
import sim.util.Int2D

import com.pathdependent.mason.ext.{BallooningPortrayal, PBM}
import com.pathdependent.mason.ext.Helpers.makeSteppable

/*
        ___ _  _ _ ____    _ ____    _  _ ____ ___    ___  ____ _   _ 
         |  |__| | [__     | [__     |\ | |  |  |     |  \ |__/  \_/  
         |  |  | | ___]    | ___]    | \| |__|  |     |__/ |  \   |   
         
  I copy / pasted SugarDynamics, then editied. Yuck! I wish Scala had macros!
  
 */

/**
 * Mixes in spice extraction, metabolism, and consumption into an agent.
 */
trait SpiceConsumption extends Agent {
  /**
   * Requires that the environment has SpiceResources.
   */
  type ET <: SpiceResources
  
  /**
   * The basal spice rate is the natural rate at which agents burn through
   * their accumulated spice. 
   */
  val basalSpiceMetabolism: Double
  
  /**
   * An agent begins life with an endowment of spice. 
   */
  val spiceEndowment: Double
  
  /**
   * An agent may accumulate spice by harvesting, trade, combat, etc.
   */ 
  @BeanProperty var accumulatedSpice: Double = spiceEndowment
  
  /**
   * Override this method if you want to affect the metabolic rate. For 
   * example, if the agent is sick, increase the metabolic rate.
   *
   * @return the current metabolic rate of spice.
   */
  def spiceMetabolicRate(): Double = basalSpiceMetabolism
  
  
  /**
   * Burns through accumulated spice.
   * 
   * Override method method if you want to affect what happens when spice is
   * metabolized. For example, produce pollution on metabolism.
   */
  def metabolizeSpice(): Double = { 
    val amount = spiceMetabolicRate() 
    accumulatedSpice -= amount
    amount
  }
  
  /**
   * An agent is dead if it's spice demands are not fullfilled -- it starves.
   */
  def hasDiedOfSpiceStarvation(): Boolean = accumulatedSpice <= 0

  /** 
   * Adds hasDiedOfSpiceStarvation to the hasDied chain.
   */
  override def hasDied() = hasDiedOfSpiceStarvation || super.hasDied()
  
  /** 
   * Adds spice extraction to the interaction chain.
   */
  override def interact(sugarscape: ET) { 
    accumulatedSpice += sugarscape.extractSpice(location)
    
    super.interact(sugarscape)
  }
  
  
  /** 
   * Adds spice metabolism to the autonomic chain.
   */
  override def updateAutonomicState(sugarscape: ET) { 
    metabolizeSpice()
    
    super.updateAutonomicState(sugarscape)
  }
}

/** 
 * Spice is a consumable resource. 
 *
 * @see Growing Artificial Socieites, p.96.
 */
trait SpiceResources extends Sugarscape {
  var spice: ObjectGrid2D = null
  
  /** 
   * Sets up the spice resources. Called upon initialization. 
   *
   * Override this method for different spice landscapes.
   */
  def configureSpice(): Unit

  /**
   * The maximum possible capacity for a spice resource.
   * 
   * This is a cached helper variable, used for rendering the 
   * BallooningPortrayals of spice.  
   *
   * @see SpicePortrayal#setupPortrayalChain()
   */
  var maxSpiceCapacity = 0.0
  
  /**
   * The sum of all spice capacities.
   *
   * A useful metric to observe. Since it is determined on initialization 
   * it's not expensive.
   */
  var totalSpiceCapacity = 0.0
  
  /**
   * Initializes the landscape of spice. Schedules growback.
   */
  override def fieldInitializerChain() {
    super.fieldInitializerChain()
    
    spice = new ObjectGrid2D(width, height)
    configureSpice()
    determineMaxSpiceCapacity()
    
    schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.ResourceGrowback,
      makeSteppable[SpiceResources](_.spiceGrowback())
    )
  }
  
  /** 
   * Grows back all the spice resources, based on the spiceGrowbackRule.
   */
  protected def spiceGrowback() {
    allLocations.foreach { 
      location =>
        val resource = spiceAt(location)
        spiceGrowbackRule(location, resource)
    }
  }
  
  /**
   * Grows back spice.
   *
   * The default is for unit growback, which is rule G∞.
   */
  protected 
  def spiceGrowbackRule(location: Int2D, resource: Resource) {
    resource.unitGrowback()
  }
  
  /**
   * Removes the spice, and returns the amount removed.
   *
   * @note by overriding or extending this method, you can add consequences
   *       to spice extraction.
   */
  def extractSpice(location: Int2D): Double = {
    val resource = spice.get(location.x, location.y).asInstanceOf[Resource]
    val amount = resource.level
    resource.level = 0
    amount
  }
  
  
  def spiceAt(location: Int2D): Resource = {
    spice.get(location.x, location.y).asInstanceOf[Resource]
  }
  
  /**
   * This is important for using the ballooning portrayal.
   */
  def determineMaxSpiceCapacity() { 
    allLocations.foreach {
      location =>
        var resource = spiceAt(location)
        maxSpiceCapacity = maxSpiceCapacity max resource.level
        totalSpiceCapacity += resource.capacity
    }
  }
}

/**
 * An reflection of the TwoSugarMountains layout.
 *
 * <b>This is not as per Growing Artificial Societies</b>
 */
trait TwoSpiceMountains extends SpiceResources {
  def configureSpice() {
    require(
      width == 50 && height == 50,
      "The TwoSpiceMountain trait requires a width of 50 and height of 50."
    )
    
    val capacities = PBM.parse(
      getClass.getResource("/sugarscape/TwoSugarMountains.pbm")
    )
    for(x <- 0 until width; y <- 0 until height; level = capacities(x, y)){
      // x,y => y,x to invert the mountaints
      spice.set(y, x, new Resource(level = level, capacity = level))
    }
  }
}

/**
 * Initially, the south experiences winter and the north experiences summer. 
 *
 * @see Growing Artificial Societies, p.44.
 */
trait SeasonalSpice extends SpiceResources {
  /**
   * The duration of each season in steps.
   */
  var durationOfSpiceSeason: Int
  
  var winterSpiceGrowbackRate: Double
  
  var summerSpiceGrowbackRate: Double
  
  override def spiceGrowbackRule(location: Int2D, resource: Resource) {
    resource.simpleSeasonalGrowback(
      isNorth = location.y < height / 2,
      timeInSteps = schedule.getSteps(),
      durationOfSeasonInSteps = durationOfSpiceSeason,
      summerGrowbackRate = summerSpiceGrowbackRate,
      winterGrowbackRate = winterSpiceGrowbackRate
    )
  }
}

/**
 * Implements the visualization of spice as Balloooning portrayals.
 */
trait SpicePortrayal extends SugarscapeWithUI {
  type ST <: SpiceResources  
  
  val spicePortrayal = new ObjectGridPortrayal2D()
  
  /**
   * Sets up the spicePortrayal.
   */
  override def setupPortrayal(sugarscape: ST) {
    super.setupPortrayal(sugarscape)
    
    spicePortrayal.setPortrayalForAll(
      BallooningPortrayal(java.awt.Color.green) { resource: Resource =>
        if(resource.level == 0.0){
          0.0
        } else {
          0.25 + 0.75 * resource.level / sugarscape.maxSpiceCapacity
        }
      }
    )
    
    spicePortrayal.setField(sugarscape.spice)
  }
  
  /** 
   * Attaches the spicePortrayal.
   */
  override def setupDisplay(display: sim.display.Display2D) {
    super.setupDisplay(display)
    
    display.attach(spicePortrayal, "The Spice")
  }
}
