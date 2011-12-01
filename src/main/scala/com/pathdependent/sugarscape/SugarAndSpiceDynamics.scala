/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import java.awt.Color
import sim.portrayal.grid.ObjectGridPortrayal2D
import com.pathdependent.mason.ext.{BallooningSemiCirclePortrayal, PBM}

/**
 * Implements the visualization of sugar as Balloooning portrayals.
 */
trait SugarAndSpicePortrayal extends SugarscapeWithUI {
  type ST <: SugarResources with SpiceResources 
  
  val sugarPortrayal = new ObjectGridPortrayal2D()
  val spicePortrayal = new ObjectGridPortrayal2D()
  
  /**
   * Sets up the sugarPortrayal.
   */
  override def setupPortrayal(sugarscape: ST) {
    super.setupPortrayal(sugarscape)
    
    sugarPortrayal.setPortrayalForAll(
      BallooningSemiCirclePortrayal(45, 180, Color.blue) { resource: Resource =>
        if(resource.level == 0.0){
          0.0
        } else {
          0.25 + 0.75 * resource.level / sugarscape.maxSugarCapacity
        }
      }
    )
    
    val blueViolet = new Color(138, 43, 226)
    spicePortrayal.setPortrayalForAll(
      BallooningSemiCirclePortrayal(225, 180, blueViolet) { resource: Resource =>
        if(resource.level == 0.0){
          0.0
        } else {
          0.25 + 0.75 * resource.level / sugarscape.maxSugarCapacity
        }
      }
    )
    
    sugarPortrayal.setField(sugarscape.sugar)
    spicePortrayal.setField(sugarscape.spice)
  }
  
  /** 
   * Attaches the sugarPortrayal.
   */
  override def setupDisplay(display: sim.display.Display2D) {
    super.setupDisplay(display)
    
    display.attach(sugarPortrayal, "The Sugar")
    display.attach(spicePortrayal, "The Spice")
  }
}
