/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import com.pathdependent.mason.ext.BinaryString

/**
 * Embues an agent with a binary culture.
 *
 * @see Growing Artificial Societies, p.73.
 */
trait Culture extends Agent with GroupIdentity {
  val initialCulturalTags: BinaryString
  val culturalTags: BinaryString = initialCulturalTags.duplicate 
}

trait RuleK extends Culture {
  /**
   * Tag majority group membership rule.
   * 
   * @see Growing Artificial Societies, p.73.
   */
  def groupIdentity(): Int = if(culturalTags.fractionOfTrueBits > 0.5) 1 else 0
  
  override def interact(sugarscape: ET) { 
    transmitCultureByTagFlipping(sugarscape)
    
    super.interact(sugarscape)
  }
  
  def transmitCultureByTagFlipping(sugarscape: ET) {
    def flipTags(neighbor: Culture) {
      val tagIndex = sugarscape.random.nextInt(culturalTags.length)
      
      neighbor.culturalTags(tagIndex) = culturalTags(tagIndex)
    }
    
    immediateNeighborhood(sugarscape.random).
      map { loc => sugarscape.agentAt(sugarscape.translateLocation(loc))}.
      filterNot( _ == null).
      foreach { agent => flipTags(agent.asInstanceOf[Culture]) }
  }
}


