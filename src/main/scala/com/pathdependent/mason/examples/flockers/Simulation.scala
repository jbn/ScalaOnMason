/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.examples.flockers

import scala.collection.JavaConversions._

import sim.engine.{SimState, Steppable}
import sim.field.continuous.Continuous2D
import sim.util.{Double2D,Int2D}

/**
 * Flockers is the name of this simulation. MASON allows for full seperation
 * of the model (simulation) from a graphical viewer. (Although, I'm a bit 
 * hesitant to call it an MVC like they do; they could have gone further
 * in separating the controller bits.)
 *
 * There is not much to this simulation. The only field (in MASON parlance)
 * in the original simulation was the flock. (In the reference implementation,
 * this was called flockers.) 
 */
class Flockers(seed: Long) extends SimState(seed) with VisitationRecord {  
  val params = FlockerParameters()
  var flock: Continuous2D = null
  
  /**
   * @return the locations of all living agents in discrete terms.
   */
  def discreteLivingLocations(): List[Int2D] = {
    flock.getAllObjects.filterNot(_.asInstanceOf[Flocker].dead).map {
      obj => 
        val location = flock.getObjectLocation(obj)
        new Int2D(location.x.toInt, location.y.toInt)
    }.toList
  }
  
  override def start() {
    super.start() 
    
    val radiansOfCircle = 2.0 * math.Pi
    
    // Scala allows you to import pretty much anything. The compiler will
    // issue a warning for shadowed variables.
    import params.{
      flockSize, width, height, neighborhoodRadius, deadFlockerProbability
    }

    // The flock exists in a Continuous2D space.     
    flock = new Continuous2D(neighborhoodRadius / 1.5, width, height)
    
    for(i <- 0 until flockSize) {
      val flocker = new Flocker(
        new Double2D(random.nextDouble * width, random.nextDouble * height),
        random.nextBoolean(deadFlockerProbability),
        random.nextDouble * radiansOfCircle
      )
      
      flock.setObjectLocation(flocker, flocker.location)
      schedule.scheduleRepeating(1, flocker)
    }
    
    setupVisitationRecord(width, height, schedule)
  }
}

object Flockers {
  def main(args: Array[String]) {
    SimState.doLoop(classOf[Flockers], args)
    System.exit(0)
  }
}

