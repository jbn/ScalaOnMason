/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import sim.engine.{Steppable, SimState}
import sim.field.grid.DoubleGrid2D
import sim.portrayal.grid.ValueGridPortrayal2D
import sim.portrayal.simple.RectanglePortrayal2D
import sim.util.{Int2D, IntBag, DoubleBag}


import com.pathdependent.mason.ext.Helpers.{makeSteppable,sum}

/**
 * 
 */
trait MovementRuleMModifiedForPollution extends Agent { 
  self: SugarConsumption =>
  
  /**
   * Requires that the environment has SugarResources.
   *
   * @todo fix bug related to ET bounds being overridden.
   */
  type ET <: Pollution
  
  /**
   * This is the agent movement rule, M, modified for pollution. The following 
   * is taken directly from GAS.
   *
   * <ul>
   *   <li>Look out as far as vision permits in the four principal lattice
   *       directions and identify the unoccupied site(s) having the maximum
   *       <i>sugar to pollution ratio</i>;</li>
   *   <li>If the maximum <i>sugar to pollution ratio</i> appears on multiple 
   *       sites then select the nearest one;</li>
   *   <li>Move to this site;</li>
   *   <li>Collect all the sugar at this new position</li>
   * </ul>
   *
   * @see Growing Artificial Societies, pp. 48, 49
   */
  def identifyBestLocation(sugarscape: ET): Option[Int2D] = {
    def sugarToPollutionRatio(location: Int2D): Double = {
      sugarscape.sugarAt(location).level / 
        (sugarscape.asInstanceOf[Pollution].pollutionAt(location) + 1.0) // XXX
    }
    
    val neighborhood = neighborhoodLocations(sugarscape.random) 
    
    val emptyOrderedPositions = neighborhood.
      map { sugarscape.translateLocation(_) }. // clip or wrap coords.
      filterNot { sugarscape.isOccupied(_) }.  // limit to unoccupied site(s)
      sortWith {                               // sort by descending sugar level
        (a, b) => sugarToPollutionRatio(a) > sugarToPollutionRatio(b)
      } 

    if(emptyOrderedPositions.isEmpty) {
      None
    } else {
      val maxRatio = sugarToPollutionRatio(emptyOrderedPositions.head)
      
      // Limit to the positions with the max ratio.
      val bestPositions = emptyOrderedPositions.filter {
        sugarToPollutionRatio(_) == maxRatio
      }
      
      // Find nearest location.
      val destination = bestPositions.sortWith {
        (a,b) => a.manhattanDistance(location) < b.manhattanDistance(location)
      }.head
      
      Some(destination)
    }
  }
}

trait PollutionGeneratedByConsumptionOfSugar extends SugarConsumption {
  // XXX FUCK
  // type ET <: Pollution

  var pollutionGeneratedBySugarExtraction: Double  
  var sugarRecentlyMetabolized = 0.0
  
  override def metabolizeSugar(): Double = { 
    val amount = super.metabolizeSugar()
    sugarRecentlyMetabolized = amount
    amount
  }
  
  /** 
   * Since we are extending Sugarconsumption, metabolism will come first,
   * ...right...
   */
  override def updateAutonomicState(sugarscape: ET) { 
    super.updateAutonomicState(sugarscape)
    
    val pollutionScape = sugarscape.asInstanceOf[Pollution]
    
    pollutionScape.pollution.set(
      location.x, location.y, 
      pollutionScape.pollutionAt(location) +
        sugarRecentlyMetabolized * pollutionGeneratedBySugarExtraction
    )
  }
}

trait Pollution extends Sugarscape {
  var pollution: DoubleGrid2D = null
  
  /**
   * Initializes the atmospheric pollution and schedules dissipation.
   */
  override def fieldInitializerChain() {
    super.fieldInitializerChain()

    // There is no pollution in the beginning. It's eden.    
    pollution = new DoubleGrid2D(width, height)
    
    schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.ResourceGrowback,
      makeSteppable[Pollution](_.pollutionDissipation())
    )
  }
  
  /**
   * Pollution can also NOT dissipate, which may make interesting long term
   * patterns.
   */
  def pollutionDissipation() {}
  
  def pollutionAt(location: Int2D) = pollution.get(location.x, location.y)
}

trait PollutionDiffusion extends Pollution {
  /**
   * Diffusion takes place every diffusionInterval steps.
   * 
   * @see Growing Artificial Societies, p.48.
   */
  var diffusionInterval: Int

  /**
   * This is used as a temporary variable for calculating the flux in a CA
   * synchronous style.
   */  
  var pollutionFlux: DoubleGrid2D = null
  
  override def fieldInitializerChain() {
    super.fieldInitializerChain()
    
    pollutionFlux = new DoubleGrid2D(width, height)
  }
  
  override def pollutionDissipation() {
    // Since I am forced to use MASON's bags for collecting neighbors, 
    // I might as adhere to its recommendation of scratch bags.
    var levels = new DoubleBag()
    var xs, ys = new IntBag
        
    if(schedule.getSteps() % diffusionInterval == 0) {
      // First, compute the pollution flux for each location. 
      allLocations.foreach{ location =>
        pollution.getNeighborsMaxDistance(
          location.x, location.y, 1, true, levels, xs, ys
        )
        
        pollutionFlux.set(
          location.x, location.y,
          sum(levels) / levels.numObjs
        )
      }
      
      // Now, update the actual levels.
      allLocations.foreach{ location =>
        pollution.set(
          location.x, location.y, 
          pollutionFlux.get(location.x, location.y)
        )
      }
    }
  }
}

trait PollutionGeneratedByExtractionOfSugar extends Pollution with SugarResources {
  self: SugarResources =>
  
  var pollutionGeneratedBySugarExtraction: Double
  
  override def extractSugar(location: Int2D): Double = {
    val amount = super.extractSugar(location)
    
    pollution.set(
      location.x, location.y, 
      pollutionAt(location) + amount * pollutionGeneratedBySugarExtraction
    )
    
    amount
  }
}

trait PollutionPortrayal extends SugarscapeWithUI  {
  type ST <: SugarResources with Pollution
 
  var maxObservedPollution = 0.0 
  var minObservedPollution = 0.0
  var pollutionScalar = 0.0
  val pollutionPortrayal = new ValueGridPortrayal2D()
  
  /**
   * Sets up the sugarPortrayal.
   */
  override def setupPortrayal(sugarscape: ST) {
    super.setupPortrayal(sugarscape)
    
    pollutionScalar = 0.0
    maxObservedPollution = 0.0 
    minObservedPollution = 0.0
    
    sugarscape.schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.Statistics,
      makeSteppable[Pollution]{ sugarscape => 
          maxObservedPollution = sugarscape.pollution.max
          minObservedPollution = sugarscape.pollution.min
          pollutionScalar = 
            if(maxObservedPollution == 0.0) 0.0 else 1.0 / maxObservedPollution
          
      }
    )
    
    pollutionPortrayal.setPortrayalForAll(
      new RectanglePortrayal2D {
        override def draw(obj: Object, g: java.awt.Graphics2D, info: sim.portrayal.DrawInfo2D) {
          val x = obj.asInstanceOf[sim.util.MutableDouble].`val`
          paint = new java.awt.Color(
            255, 0, 0, 
            ((x - minObservedPollution) * pollutionScalar * 255 * 0.9).toInt
          )
          super.draw(obj, g, info)
        }
      }
    )
    
    pollutionPortrayal.setField(sugarscape.pollution)
  }
  
  /** 
   * Attaches the sugarPortrayal.
   */
  override def setupDisplay(display: sim.display.Display2D) {
    super.setupDisplay(display)
    
    display.attach(pollutionPortrayal, "The Pollution")
  }
}


