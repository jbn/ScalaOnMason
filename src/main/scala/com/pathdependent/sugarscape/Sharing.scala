/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import com.pathdependent.sugarscape._

import scala.reflect.{BeanProperty}

import sim.engine.{SimState}
import sim.util.{Int2D, Interval}
import ec.util.MersenneTwisterFast

import com.pathdependent.mason.ext.PBM
import com.pathdependent.mason.ext.Helpers.{makeSteppable}



trait SugarSharing extends SugarConsumption with GroupIdentity {
  type AT <: SugarSharing
  /**
   * Between 0 and 1.0, 0 denoting no sharing, 100% denoting 
   * this agent will last only one step.
   */
  val sugarSharingGenerosity: Double
  
  val fairnessEnforced: Boolean
  
  def shareSugar(partner: SugarSharing) {
    def timeUntilStarvation(agent: SugarSharing) = {
      agent.accumulatedSugar / agent.basalSugarMetabolism
    }
    
    val t1 = timeUntilStarvation(this)
    val t2 = timeUntilStarvation(partner)

    /*
    // Only share with agents who have similar ideas on sharing.
    val fairnessCheck = 
      if(fairnessEnforced) (partner.sugarSharingGenerosity - 0.1) > sugarSharingGenerosity
      else true
      
    // Sharing can take place.
    // This is for sugarSharingGenerosity = 100%
    if(t1 > t2 && sugarSharingGenerosity > 0.0 && fairnessCheck) {
      val sharableSugar = accumulatedSugar * sugarSharingGenerosity
      val guardedSugar = accumulatedSugar - sharableSugar
        
      val jointSugar = sharableSugar + partner.accumulatedSugar
      val jointMetabolism = basalSugarMetabolism + partner.basalSugarMetabolism
     
      val nextSugar = basalSugarMetabolism / jointMetabolism * jointSugar + guardedSugar
      val partnerNextSugar = partner.basalSugarMetabolism / jointMetabolism * jointSugar
        
      // Need to survive at least one step
      if(nextSugar > basalSugarMetabolism && partnerNextSugar > partner.accumulatedSugar) { 
        accumulatedSugar = nextSugar
        partner.accumulatedSugar = partnerNextSugar

      }
    }*/
    if(t1 > t2 && sugarSharingGenerosity > 0.0) {
      val r = if(fairnessEnforced) {
        sugarSharingGenerosity min partner.sugarSharingGenerosity
      } else sugarSharingGenerosity
      
      val sharableSugar = accumulatedSugar * r
      val guardedSugar = accumulatedSugar - sharableSugar
        
      val jointSugar = sharableSugar + partner.accumulatedSugar
      val jointMetabolism = basalSugarMetabolism + partner.basalSugarMetabolism
     
      val nextSugar = basalSugarMetabolism / jointMetabolism * jointSugar + guardedSugar
      val partnerNextSugar = partner.basalSugarMetabolism / jointMetabolism * jointSugar
        
      // Need to survive at least one step
      if(nextSugar > basalSugarMetabolism && partnerNextSugar > partner.accumulatedSugar) { 
        accumulatedSugar = nextSugar
        partner.accumulatedSugar = partnerNextSugar

      }
    }
  }
  
  /** 
   * You only share if you have *more* than them. 
   */
  override def interact(sugarscape: ET) { 
    // Get neighbors that are in your group.
    val tradePartners = immediateNeighborhood(sugarscape.random).
      map(l=>sugarscape.agentAt(sugarscape.translateLocation(l)).asInstanceOf[AT]).
      filter { agent => agent != null && belongsToSameGroupAs(agent) }

    tradePartners.foreach{ agent => shareSugar(agent.asInstanceOf[AT]) }
    
    super.interact(sugarscape)
  }
}
