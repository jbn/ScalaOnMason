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

/**
 * Mixes in sugar extraction, metabolism, and consumption into an agent.
 */
trait SugarConsumption extends Agent {
  /**
   * Requires that the environment has SugarResources.
   */
  type ET <: SugarResources
  
  /**
   * The basal sugar rate is the natural rate at which agents burn through
   * their accumulated sugar. 
   */
  val basalSugarMetabolism: Double
  
  /**
   * An agent begins life with an endowment of sugar. 
   */
  val sugarEndowment: Double
  
  /**
   * An agent may accumulate sugar by harvesting, trade, combat, etc.
   *
   * Initially, it equals their endowment of sugar.
   */ 
  @BeanProperty var accumulatedSugar: Double = sugarEndowment
  
  /**
   * Override this method if you want to affect the metabolic rate. For 
   * example, if the agent is sick, increase the metabolic rate.
   *
   * @return the current metabolic rate of sugar.
   */
  def sugarMetabolicRate(): Double = basalSugarMetabolism
  
  /**
   * Burns through accumulated sugar.
   * 
   * Override method method if you want to affect what happens when sugar is
   * metabolized. For example, produce pollution on metabolism.
   */
  def metabolizeSugar() { accumulatedSugar -= sugarMetabolicRate() }
  
  /**
   * An agent is dead if it's sugar demands are not fullfilled -- it starves.
   *
   * @note I initially implemented this function with less than 0 as the
   *       predicate, thinking an agent may have just metabolized all it's 
   *       sugar; is currently statisfied; but will starve if it doesn't gain
   *       resources the next step. In GAS, an agent starves if it hits zero.
   *
   * @see Growing Artificial Socieites, p.25. 
   */
  def hasDiedOfSugarStarvation(): Boolean = accumulatedSugar <= 0

  /**
   * This is the agent movement rule, M. The following is taken directly from
   * GAS.
   *
   * <ul>
   *   <li>Look out as far as vision permits in the four principal lattice
   *       directions and identify the unoccupied site(s) having the most
   *       sugar;</li>
   *   <li>If the greatest sugar value appears on multiple sites than select
   *       the nearest one;</li>
   *   <li>Move to this site;</li>
   *   <li>Collect all the sugar at this new position</li>
   * </ul>
   *
   * I have chosen to separate extraction behavior from movement behavior. 
   * Extraction can be a fun operation to extend.
   *
   * @return optionally the desired location. None is returned if none are
   *         desirable. This was not mentioned in GAS, but it seems possible
   *         that an agent can opt NOT to move.
   *
   * @see Growing Artificial Societies, p.25.
   */
  def mostDesirableSugarLocation(sugarscape: ET): Option[Int2D] = {
    val neighborhood = neighborhoodLocations(sugarscape.random) 
    
    val emptyOrderedPositions = neighborhood.
      map { sugarscape.translateLocation(_) }. // clip or wrap coords.
      filterNot { sugarscape.isOccupied(_) }.  // limit to unoccupied site(s)
      sortWith {                               // sort by descending sugar level
        (a, b) => sugarscape.sugarAt(a).level > sugarscape.sugarAt(b).level
      } 

    if(emptyOrderedPositions.isEmpty) {
      None
    } else {
      val maxSugarLevel = sugarscape.sugarAt(emptyOrderedPositions.head).level
      
      // Limit to the positions with the maximum level of sugar.
      val bestPositions = emptyOrderedPositions.filter {
        sugarscape.sugarAt(_).level == maxSugarLevel
      }
      
      // Find nearest location.
      val destination = bestPositions.sortWith {
        (a,b) => a.manhattanDistance(location) < b.manhattanDistance(location)
      }.head
      
      Some(destination)
    }
  }

  /** 
   * Adds hasDiedOfSugarStarvation to the hasDied chain.
   */
  override def hasDied() = hasDiedOfSugarStarvation || super.hasDied()
  
  /** 
   * Adds sugar extraction to the interaction chain.
   */
  override def interact(sugarscape: ET) { 
    accumulatedSugar += sugarscape.extractSugar(location)
    
    super.interact(sugarscape)
  }
  
  
  /** 
   * Adds sugar metabolism to the autonomic chain.
   */
  override def updateAutonomicState() { 
    metabolizeSugar()
    
    super.updateAutonomicState()
  }
}

/** 
 * Makes the agent move according to the Rule M. 
 */
trait MovementRuleM { self: SugarConsumption =>
  def identifyBestLocation(sugarscape: ET) = {
    mostDesirableSugarLocation(sugarscape)
  }
}

/** 
 * Sugar is a consumable resource. 
 *
 * @see Growing Artificial Socieites, p.21.
 */
trait SugarResources extends Sugarscape {
  var sugar: ObjectGrid2D = null
  
  /** 
   * Sets up the sugar resources. Called upon initialization. 
   *
   * Override this method for different sugar landscapes.
   */
  def configureSugar(): Unit

  /**
   * The maximum possible capacity for a sugar resource.
   * 
   * This is a cached helper variable, used for rendering the 
   * BallooningPortrayals of sugar.  
   *
   * @see SugarPortrayal#setupPortrayalChain()
   */
  var maxSugarCapacity = 0.0
  
  /**
   * The sum of all sugar capacities.
   *
   * A useful metric to observe. Since it is determined on initialization 
   * it's not expensive.
   */
  var totalSugarCapacity = 0.0
  
  /**
   * Initializes the landscape of sugar. Schedules growback.
   */
  override def fieldInitializerChain() {
    super.fieldInitializerChain()
    
    sugar = new ObjectGrid2D(width, height)
    configureSugar()
    determineMaxSugarCapacity()
    
    schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.ResourceGrowback,
      makeSteppable[SugarResources](_.sugarGrowback())
    )
  }
  
  /** 
   * Grows back all the sugar resources, based on the sugarGrowbackRule.
   */
  protected def sugarGrowback() {
    allLocations.foreach { 
      location =>
        val resource = sugarAt(location)
        resource.level = sugarGrowbackRule(location, resource)
    }
  }
  
  /**
   * Grows back sugar.
   *
   * The default is for unit growback, which is rule G∞.
   *
   * @see Growing Artifical Societies, p.26.
   */
  protected 
  def sugarGrowbackRule(location: Int2D, resource: Resource): Double = {
    Resource.CapacityLimited(resource, Resource.UnitGrowback)
  }
  
  /**
   * Removes the sugar, and returns the amount removed.
   *
   * @note by overriding or extending this method, you can add consequences
   *       to sugar extraction.
   */
  def extractSugar(location: Int2D): Double = {
    val resource = sugar.get(location.x, location.y).asInstanceOf[Resource]
    val amount = resource.level
    resource.level = 0
    amount
  }
  
  
  def sugarAt(location: Int2D): Resource = {
    sugar.get(location.x, location.y).asInstanceOf[Resource]
  }
  
  /**
   * This is important for using the ballooning portrayal.
   */
  def determineMaxSugarCapacity() { 
    allLocations.foreach {
      location =>
        var resource = sugarAt(location)
        maxSugarCapacity = maxSugarCapacity max resource.level
        totalSugarCapacity += resource.capacity
    }
  }
}

/**
 * Implements the ubiqutious Two Sugar Mountains, as illustrated by 
 * Growing Artificial Societies. 
 *
 * @see Growing Artificial Societies, p.22.
 */
trait TwoSugarMountains extends SugarResources {
  def configureSugar() {
    require(
      width == 50 && height == 50,
      "The TwoSugarMountain trait requires a width of 50 and height of 50."
    )
    
    val capacities = PBM.parse(getClass.getResource("/TwoSugarMountains.pbm"))
    for(x <- 0 until width; y <- 0 until height; level = capacities(x, y)){
      sugar.set(x, y, new Resource(level = level, capacity = level))
    }
  }
}

/**
 * Initially, the south experiences winter and the north experiences summer. 
 *
 * @see Growing Artificial Societies, p.44.
 */
trait SeasonalSugar extends SugarResources {
  /**
   * The duration of each season in steps.
   */
  var durationOfSugarSeason: Int
  var sugarSummerGrowbackRule: Resource.GrowbackRule
  var sugarWinterGrowbackRule: Resource.GrowbackRule
  
  override def sugarGrowbackRule(location: Int2D, resource: Resource): Double ={
    val firstSeason = schedule.getSteps % 
      (2 * durationOfSugarSeason) / durationOfSugarSeason < 1
      
    val growbackRule = 
      if(location.y < height / 2) { 
        if(firstSeason) sugarSummerGrowbackRule else sugarWinterGrowbackRule
      } else {
        if(firstSeason) sugarWinterGrowbackRule else sugarSummerGrowbackRule
      }
      
    Resource.CapacityLimited(resource, growbackRule)
  }
}

/**
 * Implements the visualization of sugar as Balloooning portrayals.
 */
trait SugarPortrayal extends SugarscapeWithUI {
  type ST <: SugarResources  
  
  val sugarPortrayal = new ObjectGridPortrayal2D()
  
  /**
   * Sets up the sugarPortrayal.
   */
  override def setupPortrayal(sugarscape: ST) {
    super.setupPortrayal(sugarscape)
    
    sugarPortrayal.setPortrayalForAll(
      BallooningPortrayal(java.awt.Color.blue) { resource: Resource =>
        if(resource.level == 0.0){
          0.0
        } else {
          0.25 + 0.75 * resource.level / sugarscape.maxSugarCapacity
        }
      }
    )
    
    sugarPortrayal.setField(sugarscape.sugar)
  }
  
  /** 
   * Attaches the sugarPortrayal.
   */
  override def setupDisplay(display: sim.display.Display2D) {
    display.attach(sugarPortrayal, "The Sugar")
  }
}
