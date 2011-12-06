/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable
import scala.reflect.BooleanBeanProperty

import sim.engine.{Schedule, SimState, Steppable}
import sim.util.Int2D
import ec.util.MersenneTwisterFast
import sim.portrayal.grid.{ObjectGridPortrayal2D}
import sim.portrayal.simple.{OvalPortrayal2D}
import java.awt.Color

import com.pathdependent.mason.ext.Helpers.shuffle

abstract class Agent extends Steppable {
  type AT <: Agent
  
  /**
   * The type of environment this agent interacts with and within. By defining
   * an abstract type, different traits can specify their requirements in the
   * form of type bounds on ET. 
   *
   * For example, an agent with Sugar requirements can specify that the 
   * environment must have Sugar resources. Scala's elegant type system can be
   * used to enforce <i>semantic</i> properties, resulting in simulations
   * that are difficult -- but certainly not impossible -- to construct 
   * incorrectly.
   */
  type ET <: Sugarscape

  /** 
   * This could technically be extracted to a trait, but I think every
   * configuration would need vision, otherwise the agent has no perception
   * of the environment.
   */
  val depthOfVision: Int
  
  /**
   * Agents with Aging can be instantiated with a non-zero age. Consequently, 
   * their age would not be a true reflection of their existence in the 
   * simulation.
   */
  var stepsCompleted = 0
  
  /**
   * I really dislike storing the location in an agent. It's yet another
   * error-prone pattern that needs correcting.
   */
  var location: Int2D = null
  
  /**
   * Each agent has it's own internal schedule.
   * Bigbee instead used a sim.engine.Sequence and the order was guarenteed.
   * 
   */
  private val tasks = new Schedule()
  
  /** 
   * Predicate that indicates whether an agent is alive or dead.
   *
   * This method is meant to be extended and <i>chained</i> when mixing in new 
   * behaviors and requirements. 
   *
   * Proper extension can be as simple as:
   *
   * {{{ 
   * override def hasDied() = extentionPredicateOfLife || super.hasDied() 
   * }}}
   * 
   * @note If you don't chain it with super calls, you're breaking things. I 
   *       wish scala had a chained keyword instead of override that would 
   *       throw a compiler error if there was no path to the root. 
   *
   * @see SugarDynamics.scala for a working example.
   */
  def hasDied(): Boolean = isDead
  
  /**
   * This value is set to true automatically once an agent dies; however, it
   * also loops back into the execution chain by way of the base method so you 
   * can induce agent death from the GUI. 
   */
  @BooleanBeanProperty var isDead = false
  
  /**
   * Chained hook for updating autonomic state variables. 
   *
   * Examples include advancing the age, metabolizing sugar, etc.
   * 
   * @note This might not work out, since it makes reordering updates
   *       difficult...
   */
  def updateAutonomicState(sugarscape: ET) { }
  
  /**
   * Chained hook for any type of interaction. Interactions take place after 
   * the agent moves.
   *
   * @note This might not work out, since it makes reordering interactions
   *       difficult...
   */
  def interact(sugarscape: ET) { }

  /**
   * @note Override this method for different movement rules.
   *
   * @return the most desirable location, from this agent's viewpoint or
   *         None if it does not see an attractive location.
   */
  def identifyBestLocation(sugarscape: ET): Option[Int2D]
  
  /**
   * As per MASON's design, this is where the action occurs on each step.
   */
  def step(uglyGenericState: SimState){
    val sugarscape = uglyGenericState.asInstanceOf[ET]
    
    identifyBestLocation(sugarscape).foreach{ 
      sugarscape.moveAgent(location, _) 
    }
    
    // XXX: This is a problem. Interact before update Autonomic...?
    interact(sugarscape)

    updateAutonomicState(sugarscape)
      
    if(hasDied) { 
      isDead = true
      sugarscape.reap(location) 
    } else { 
      sugarscape.schedule.scheduleOnce(
        this, Sugarscape.Ordering.AgentActivation
      )
      stepsCompleted += 1 
    }
  }

  def immediateNeighborhood(rng: MersenneTwisterFast): List[Int2D] = {
    val neighborhood = mutable.ListBuffer.empty[Int2D]

    neighborhood += new Int2D(location.x, location.y - 1) // North
    neighborhood += new Int2D(location.x + 1, location.y) // East
    neighborhood += new Int2D(location.x, location.y + 1) // South
    neighborhood += new Int2D(location.x - 1, location.y) // West

    shuffle(neighborhood, rng).toList
  }
  
  /**
   * @return randomly ordered list of locations with this agents vision
   *         limited to the "four principal lattice" directions. The locations
   *         are <em>not translated</em>. They can be out of bounds. The client
   *         must determine whether to clip or wrap.
   *
   * @see Growing Artificial Societies, p.24.
   */  
  def neighborhoodLocations(rng: MersenneTwisterFast): List[Int2D] ={
    val neighborhood = mutable.ListBuffer.empty[Int2D]

    for(distance <- 1 to depthOfVision){
      neighborhood += new Int2D(location.x, location.y - distance) // North
      neighborhood += new Int2D(location.x + distance, location.y) // East
      neighborhood += new Int2D(location.x, location.y + distance) // South
      neighborhood += new Int2D(location.x - distance, location.y) // West
    }

    shuffle(neighborhood, rng).toList
  }
}



/**
 * Implements the visualization of the agent as a yellow dot.
 */
trait AgentPortrayal extends SugarscapeWithUI {
  val agentsPortrayal = new ObjectGridPortrayal2D() 

  /**
   * Sets up the sugarPortrayal.
   */
  override def setupPortrayal(sugarscape: ST) {
    super.setupPortrayal(sugarscape)

    agentsPortrayal.setPortrayalForNonNull(
      new OvalPortrayal2D(Color.yellow, 0.2)
    )
  
    agentsPortrayal.setField(sugarscape.agents)
  }
  
  /** 
   * Attaches the sugarPortrayal.
   */
  override def setupDisplay(display: sim.display.Display2D) {
    super.setupDisplay(display)
    
    display.attach(agentsPortrayal, "The Agents")
  }
}
