/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.reflect.BeanProperty

import sim.util.Int2D

trait Inheritance extends Sugarscape {
  type AT <: SugarConsumption with Ancestry
  
  override def reap(location: Int2D) { 
    val deadAgent = agentAt(location)
    
    if(deadAgent.accumulatedSugar > 0) {
      val livingChildren = deadAgent.children.filterNot(_.isDead)
      val n = livingChildren.length
      
      if(n > 0) {
        val amount = deadAgent.accumulatedSugar / n
        livingChildren.foreach{ child => 
          child.asInstanceOf[AT].accumulatedSugar += amount        
        }
      }
    }
    
    super.reap(location)
  }
}

trait SnippedInheritance extends Inheritance {
  override def reap(location: Int2D) {
    val deadAgent = agentAt(location)
    
    super.reap(location)
    
    // This is so the GC can run...
    deadAgent.mother = None
    deadAgent.father = None
    
  }  
}
