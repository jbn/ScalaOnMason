/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import sim.util.Int2D
import ec.util.MersenneTwisterFast

import scala.reflect.BeanProperty

/**
 * Embues an agent with a sex.
 */
trait DifferentiatedSexes extends Agent {
  val sex: Sex
}

sealed abstract class Sex{
  def isMale(): Boolean
  def isFemale() = !isMale()
}
case object Male extends Sex{ def isMale() = true }
case object Female extends Sex{ def isMale() = false }

trait SexRatio extends Sugarscape {
  type AT <: DifferentiatedSexes 
  
  def getSexRatio(): Double = {
    livingAgents.count(_.sex.isMale).toDouble / livingAgents.length
  }
}
// --------------------------------------

trait SexualReproduction extends Agent {
  def isFertile(): Boolean
  def reproduce(sugarscape: ET)
}

/**
 * @see Growing Artificial Socieities, p.56.
 */
trait SexRuleS extends SexualReproduction 
  with SugarConsumption with FiniteLifespan with DifferentiatedSexes {
  
  // type ET#AT <: DifferentiatedSexes // How do I do this?
  
  val onsetAgeOfFertility: Int
  val terminalAgeOfFertility: Int
  
  def isFertile(): Boolean = {
    accumulatedSugar > sugarEndowment && 
      age >= onsetAgeOfFertility && age <= terminalAgeOfFertility
  }
  
  override def interact(sugarscape: ET) { 
    reproduce(sugarscape)
    
    super.interact(sugarscape)
  }
  
  def reproduce(sugarscape: ET): Unit = if(isFertile) {
    val nearbyLocations = 
      immediateNeighborhood(sugarscape.random).
      map { sugarscape.translateLocation }
    
    def locationForChild(): Option[Int2D] = {
      nearbyLocations.find { !sugarscape.isOccupied(_) }
    }

    // XXX: Wrong because only at least one agent needs an empty place for child

    val potentialPartners = nearbyLocations.
      filter(sugarscape.isOccupied).
      map{ sugarscape.agentAt(_).asInstanceOf[SexRuleS] }.
      filter(partner => sex != partner.sex && partner.isFertile)
    
    for(
      partner <- potentialPartners; 
      location <- locationForChild;
      if isFertile) {
      sugarscape.insertAgent(
        location, 
        spawnChild(
          sugarscape.random, partner.asInstanceOf[AT]
        ).asInstanceOf[sugarscape.AT] // XXX
      )
    }
  }
  
  /**
   * Override this method to define how the child is made up.
   */
  def spawnChild(rng: MersenneTwisterFast, partner: AT): AT 
}

/** 
 * Getting nice and sloppy now...
 */
trait SexRuleSWithAncestry extends Agent with SexRuleS with Ancestry {
  override def reproduce(sugarscape: ET): Unit = if(isFertile) {
    val nearbyLocations = immediateNeighborhood(sugarscape.random).
      map { sugarscape.translateLocation }
    
    def locationForChild(): Option[Int2D] = {
      nearbyLocations.find { !sugarscape.isOccupied(_) }
    }

    // XXX: Wrong because only at least one agent needs an empty place for child

    val potentialPartners = nearbyLocations.
      filter(sugarscape.isOccupied).
      map{ sugarscape.agentAt(_).asInstanceOf[SexRuleSWithAncestry] }.
      filter(partner => sex != partner.sex && partner.isFertile)
    
    for(
      partner <- potentialPartners; 
      location <- locationForChild;
      if isFertile) {
      val (m,f) = if(sex == Male) (partner, this) else (this, partner)
      val child = spawnChild(
        sugarscape.random, partner.asInstanceOf[AT]
      ).asInstanceOf[Ancestry] // XXX
      
      children += child.asInstanceOf[AT]
      partner.children += child.asInstanceOf[partner.AT]
      child.mother = Some(m.asInstanceOf[child.AT])
      child.father = Some(f.asInstanceOf[child.AT])
      
      sugarscape.insertAgent(location, child.asInstanceOf[sugarscape.AT])
    }
  }
}

