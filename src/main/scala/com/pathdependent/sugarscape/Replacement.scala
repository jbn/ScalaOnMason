/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import sim.util.Int2D

import com.pathdependent.mason.ext.Helpers.makeSteppable

/**
 * If a simulation has ImmediateReplacement, a new agent will be spawned and
 * randomly located immediately when any agent dies. This means the population 
 * will stay constant at all times, including within a step.
 */
trait ImmediateReplacement extends Sugarscape {
  type AT <: FiniteLifespan
  
  /**
   * @note A new agent is spawned only if their are locations available. By
   *       definition, this should be true, and the test is not required...
   *       ...I think.
   */
  override def reap(location: Int2D) { 
    super.reap(location)
    
    randomizeAllLocations()
    
    // Don't spawn if a location is unavailable. 
    allLocations.
      find { !isOccupied(_) }. // Find unoccupied location.
      foreach { unoccupiedLocation => 
        val spawnedAgent = generateAgent()
        spawnedAgent.location = unoccupiedLocation
        agents.set(unoccupiedLocation.x,unoccupiedLocation.y, spawnedAgent)
        spawnedAgent

        // XXX: It bothers me that this is duplicated.
        schedule.scheduleOnce(spawnedAgent, Sugarscape.Ordering.AgentActivation)
     }
  }
}

/**
 * If a simulation has DeferredReplacement, a new agent will be spawned and
 * randomly located after all agents have finished stepping. This means the 
 * population will stay constant at the end of every step, but not between 
 * steps. This also puts new agents in a slightly disadvantagous position,
 * since the best locations are likely to be already occupied.
 *
 * @todo Ask Axtell how his replacement rule worked. If it was deferred, it 
 *       may account for a (perhaps small) degree of the observed Gini ratios.
 */
trait DeferredReplacement extends Sugarscape {
  type AT <: FiniteLifespan
  
  var agentsNeedingReplacement: Int = 0
  
  override def reap(location: Int2D) { 
    super.reap(location)
    
    agentsNeedingReplacement += 1
  }
  
  override def fieldInitializerChain() {
    super.fieldInitializerChain()
    
    agentsNeedingReplacement = 0
    
    schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.BeforeCaches,
      makeSteppable[Sugarscape with DeferredReplacement] {
        sugarscape => 
          sugarscape.randomizeAllLocations()
          allLocations.
            filterNot{ isOccupied(_) }.
            take(agentsNeedingReplacement).
            foreach { unoccupiedLocation => 
              val spawnedAgent = generateAgent()
              spawnedAgent.location = unoccupiedLocation
              agents.set(unoccupiedLocation.x,unoccupiedLocation.y, spawnedAgent)
              spawnedAgent

              // XXX: It bothers me that this is duplicated.
              schedule.scheduleOnce(spawnedAgent, Sugarscape.Ordering.AgentActivation)
            }
        agentsNeedingReplacement = 0
      }
    )
    
  }
}
