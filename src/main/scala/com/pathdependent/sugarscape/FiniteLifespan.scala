/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.reflect.BeanProperty

/**
 * Agents can die from old age, giving them a naturally finite lifespan.
 *
 * Initially, I was going to seperate aging from age-related death. My 
 * rationale was that you can have agents that behave differently based on 
 * number of years (steps) completed without having a finite lifespan; however,
 * that functionality should make use of the Agent's stepsCompleted variable. 
 * Otherwise, aging is a process of accumulation of biological problems that
 * result in cessation of life -- that is, natural death. 
 *
 * @see Growing Artificial Societies, p.32.
 */
trait FiniteLifespan extends Agent {
  require(initialAge < ageOfExpiration)

  /**
   * Sugarscape allows agents to be instantiated <em>as if</em> they have 
   * already "lived" a certain number of years (steps). This is critical to
   * prevent cyclical artifacts due to improbable demographics. For example,
   * if all agents always start at age 0 and all agents have certain fertility
   * rules that are dependent on age, then the observed population dynamics
   * will be cyclical, at least for a while.)
   *
   * The initial age is stored as a variable, because it may be a useful
   * attribute to inspect statistically. It could potentially be used to make
   * obvious some artifacts that would otherwise be unapparent.
   */
  var initialAge: Int
  
  @BeanProperty var age: Int = initialAge
  
  /** 
   * The age at which this agent will die from "natural causes."
   */ 
  var ageOfExpiration: Int
  
  def growOlder(): Unit = age += 1 
  
  def hasDiedOfOldAge(): Boolean = age >= ageOfExpiration
  
  override def hasDied() = hasDiedOfOldAge() || super.hasDied()
  
  /** 
   * Adds aging autonomic chain.
   */
  override def updateAutonomicState() { 
    growOlder()
    
    super.updateAutonomicState()
  }
}
