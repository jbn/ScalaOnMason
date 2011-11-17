/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable
import scala.reflect.BeanProperty

import sim.engine.{Schedule, SimState, Steppable}
import sim.field.grid.{ObjectGrid2D}
import sim.util.{Interval, Int2D}

import com.pathdependent.mason.ext.Helpers.{makeSteppable, shuffle}

object Sugarscape {
  object Ordering {
    val ResourceGrowback = 0
    val AgentActivation = 1
    val BeforeCaches = 97
    val Caches = 98
    val Statistics = 99
  }
}

/**
 * @todo Find a reasonable way to making use of vals for fields. I really don't
 *       like that the SimState is the controller and the model.
 *     
 */
abstract class Sugarscape(randomSeed: Long) extends SimState(randomSeed) {  
  import Sugarscape.Ordering
  
  type AT <: Agent

  @BeanProperty var width: Int = 50
  @BeanProperty var height: Int = 50
  
  @BeanProperty var initialAgentDensity = 0.33
  def domInitialAgentDensity = new Interval(0.0, 1.0)
  
  /**
   * @note I opted for an ObjectGrid2D instead of a SparseGrid. I'm trading off
   *       memory for speed.
   */
  var agents: ObjectGrid2D = null
  
  /**
   * A list of all the locations in this simulation. Order is arbitrary and 
   * ephemeral.
   */
  var allLocations: List[Int2D] = Nil
  
  /** 
   * A list of all living agents. Populate by the last item in a schedule.
   */
  var livingAgents = mutable.ListBuffer.empty[AT]
  
  /**
   * The number of living agents, visible to the GUI. 
   */
  def getNumberOfAgents() = livingAgents.length
  
  /**
   * Generates an agent as per this simulations specifications. 
   */
  protected def generateAgent(): AT
  
  
  /** 
   * As per MASON, this is where the magic happens.
   */
  override def start() {
    super.start()

    livingAgents = mutable.ListBuffer.empty[AT]
   
    collectAllLocations()    // Used for various initialization routines.
    
    fieldInitializerChain()

    schedule.scheduleRepeating(
      0.0, Ordering.Caches, makeSteppable[Sugarscape](_.collectLivingAgents)
    )
  }
  
  /** 
   * Chained hook for the initialization of fields.
   *
   * The base definition sets up the agents field. 
   *
   * @note Having no visible agents is an obvious indicator that the base 
   *       of the chain was never called.
   */
  protected def fieldInitializerChain() {
    randomizeAllLocations()
    
    agents = new ObjectGrid2D(width, height)
    
    allLocations.
      take((initialAgentDensity * width * height).toInt).
      foreach(spawnAgent)
  }
  
  def spawnAgent(location: Int2D): AT = {
    val agent = generateAgent()
    
    agents.set(location.x, location.y, agent)
    agent.location = location
    livingAgents += agent

    // Agents start at time 1. Agent's must reschedule themselves each
    // step assuming they are alive. Alternatively, I may want to investigate
    // using a stoppable...
    schedule.scheduleOnce(agent, Sugarscape.Ordering.AgentActivation)
    
    agent
  }
  
  /**
   * Removes a dead agent from the agents field.
   *
   * @param the location of the dead agent.
   */
  def reap(location: Int2D) { agents.set(location.x, location.y, null) }
  
  /** 
   * Moves an agent in the agent field.
   *
   * @param from is the current location
   * @param to is the target location
   */
  def moveAgent(from: Int2D, to: Int2D) {
    require(agentAt(to) == null, "Can't move an agent that does not exist.")
    
    val agent = agentAt(from)

    agents.set(from.x, from.y, null) // Remove old location
    agents.set(to.x, to.y, agent)    // Move to new location
    agent.location = to              // Update the agent's ugly location cache
  }
  
 
  /**
   * The mean vision of all living agents. 
   */
  def getMeanAgentDepthOfVision(): Double = {
    if(livingAgents.length == 0){
      0.0
    } else {
      livingAgents.map(_.depthOfVision).sum / livingAgents.length.toDouble
    }
  }
  
  /**
   * @param location unadjusted location.
   * @return new location that is within the bounds of the agent grid.
   */
  def translateLocation(location: Int2D) = new Int2D(
    agents.stx(location.x), agents.sty(location.y)
  )
  
  def isOccupied(location: Int2D) = agentAt(location) != null

  def agentAt(location: Int2D): AT = { 
    agents.get(location.x, location.y).asInstanceOf[AT]
  }
  
  /**
   * Populates the livingAgents cache to all living agents.
   */
  private def collectLivingAgents() {
    livingAgents.clear()
    
    for(location <- allLocations; agent = agentAt(location); if agent != null) {
      livingAgents += agent
    }
  }
  
  /**
   * Initializes the allLocations member variable. 
   */
  private def collectAllLocations() {
    allLocations = (
      for(x <- 0 until width; y <- 0 until height) yield new Int2D(x,y)
    ).toList
  }
  
  /**
   * Randomizes the allLocations member variable.
   *
   * @return allLocations for convienence
   */
  def randomizeAllLocations(): List[Int2D] = {
    allLocations = shuffle(allLocations, random)
    allLocations
  }
}
