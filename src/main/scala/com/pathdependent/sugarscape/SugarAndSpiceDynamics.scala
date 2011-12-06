/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import java.awt.Color
import sim.util.Int2D
import sim.portrayal.grid.ObjectGridPortrayal2D
import com.pathdependent.mason.ext.{BallooningSemiCirclePortrayal, PBM}

import scala.math.pow

trait SugarAndSpiceMountains extends SugarResources with SpiceResources {
  def configureSugar() {
    require(
      width == 51 && height == 51,
      "The TwoSugarMountain trait requires a width of 50 and height of 50."
    )
    
    val capacities = PBM.parse(
      getClass.getResource("/sugarscape/TwoResourceMountains.pbm")
    )
    for(x <- 0 until width; y <- 0 until height; level = capacities(x, y)){
      sugar.set(x, y, new Resource(level = level, capacity = level))
    }
  }
  
  def configureSpice() {
    require(
      width == 51 && height == 51,
      "The TwoSpiceMountain trait requires a width of 50 and height of 50."
    )
    
    val capacities = PBM.parse(
      getClass.getResource("/sugarscape/TwoResourceMountains.pbm")
    )
    for(x <- 0 until width; y <- 0 until height; level = capacities(x, y)){
      // Flip over the verticle axis
      spice.set(width-1-x, y, new Resource(level = level, capacity = level))
    }
  }
}

trait MultiCommodityMovementRule
  extends SugarConsumption with SpiceConsumption {
  type ET <: SugarResources with SpiceResources
  /**
   * @see Growing Artificial Socieites, p.97.
   */
  def welfare(sugarWealth: Double, spiceWealth: Double): Double = {
    val mSugar = sugarMetabolicRate
    val mSpice = spiceMetabolicRate
    val mTotal = mSugar + mSpice
    
    pow(sugarWealth, mSugar / mTotal) * pow(spiceWealth, mSpice / mTotal)
  }

  def marginalRateOfSubstitution(sugarWealth: Double, spiceWealth: Double): Double = {
    (spiceWealth / spiceMetabolicRate) / (sugarWealth / sugarMetabolicRate)
  }
  
  /**
   *
   * @see Growing Artificial Societies, p.98-99.
   */
  def maximumWelfareLocation(sugarscape: ET): Option[Int2D] = { 
    def welfareAt(position: Int2D): Double = {
      welfare(
        accumulatedSugar + sugarscape.sugarAt(position).level,
        accumulatedSpice + sugarscape.spiceAt(position).level
      )
    }
    val neighborhood = neighborhoodLocations(sugarscape.random) 
    
    val emptyOrderedPositions = neighborhood.
      map { sugarscape.translateLocation(_) }. // clip or wrap coords.
      filterNot { sugarscape.isOccupied(_) }.  // limit to unoccupied site(s)
      sortWith {                               // sort by descending welfare
        (a, b) => welfareAt(a) > welfareAt(b)
      } 

    if(emptyOrderedPositions.isEmpty) {
      None
    } else {
      val maxWelfare = welfareAt(emptyOrderedPositions.head)
      
      // Limit to the positions with the maximum level of sugar.
      val bestPositions = emptyOrderedPositions.filter {
        welfareAt(_) > maxWelfare - 0.001 // Floating Point annoyances
      }
      
      // Find nearest location.
      val destination = bestPositions.sortWith {
        (a,b) => a.manhattanDistance(location) < b.manhattanDistance(location)
      }.head
      
      Some(destination)
    }
  }
  
  def identifyBestLocation(sugarscape: ET) = maximumWelfareLocation(sugarscape)  
}


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
