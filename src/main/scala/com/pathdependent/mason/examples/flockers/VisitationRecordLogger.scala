package com.pathdependent.mason.examples.flockers

import sim.engine.{SimState, Steppable, Schedule}
import sim.field.grid.DoubleGrid2D
import sim.portrayal.grid.FastValueGridPortrayal2D
import sim.display._

/**
 * @todo This file is not finished. I ran out of time and had a more pressing
 *       project to take care of. The goal is to create a more orthogonal
 *       component system for MASON, leveraging Scala.
 */
class VisitationRecordLogger extends Steppable {
  def step(state: SimState) {
    val simulation = state.asInstanceOf[Flockers]

    simulation.discreteLivingLocations.foreach { location => 
      simulation.visitationRecord.set(
        location.x, location.y, 
        simulation.visitationRecord.get(location.x, location.y) + 
          simulation.params.visitationRecordIncrement
      )
    }
        
    val max = simulation.visitationRecord.max()
    if(max > 1.0) {
      simulation.visitationRecord.multiply(1.0 / max)
    }
  }
}


trait VisitationRecord extends SimState {
  var visitationRecord: DoubleGrid2D = null
  
  def setupVisitationRecord(width: Int, height: Int, schedule: Schedule) {
    visitationRecord = new DoubleGrid2D(width, height)  
    
    val visitationRecordLogger = new VisitationRecordLogger()
    
    schedule.scheduleOnce(0.0, 0, visitationRecordLogger)
    schedule.scheduleRepeating(2, visitationRecordLogger)
  }
}

trait VisitationRecordVisualization extends GUIState {
  val visitationPortrayal = new FastValueGridPortrayal2D()
  
  def setupVisitationRecordPortrayal(simulation: VisitationRecord) {
    visitationPortrayal.setField(simulation.visitationRecord)
  }
  
  def initVisitationRecordPortryal(display: Display2D) {
    display.attach(visitationPortrayal, "Visitation Record")
  }
}
