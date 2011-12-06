/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable

trait GroupIdentity extends Agent {
  type AT <: GroupIdentity
  
  def belongsToSameGroupAs(other: AT) = groupIdentity == other.groupIdentity

  def groupIdentity(): Int 
}

trait StaticGroupIdentity extends GroupIdentity {
  val groupIdentity: Int
}

trait GroupOneRelativeDominance extends Sugarscape {
  type AT <: GroupIdentity

  def getPercentInGroupOne(): Double = {
    val numberLiving = livingAgents.length
    
    if(numberLiving > 0) {
      var numberInGroup1 = 0

      livingAgents.foreach { 
        agent => if(agent.groupIdentity == 1) numberInGroup1 += 1 
      }

      numberInGroup1.toDouble / numberLiving * 100.0
    } else {
      -1.0
    }
  }
}

