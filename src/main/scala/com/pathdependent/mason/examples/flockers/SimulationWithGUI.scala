/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.examples.flockers

import scala.collection.JavaConversions._

import javax.swing._
import java.awt.{List=>AWTList,_}

import sim.engine._
import sim.field.continuous._
import sim.portrayal._
import sim.portrayal.continuous._
import sim.portrayal.simple._
import sim.util._
import ec.util._
import sim.display._

/**
 * @note I spent a long time struggling with a really bizarre bug. Originally,
 *       my class parameter for the SimState was named state. This lead to
 *       really bizarre load / unload behavior; it gave the appearance that
 *       things were not being serialized properly, even though they were.
 *       In reality, some funky scoping things were happening. The moral
 *       of the story: don't name the SimState class parameter <i>state</i>. 
 */
class FlockersWithUI(
  rawState: SimState
) extends GUIState(rawState) with VisitationRecordVisualization {
  val flockPortrayal = new ContinuousPortrayal2D()
  val trailsPortrayal = new ContinuousPortrayal2D()
  
  var display: Display2D = null
  var displayFrame: JFrame = null
  
  def this() = this(new Flockers(System.currentTimeMillis))
  
  /** Show the Parameters object, not this. */
  override def getSimulationInspectedObject() = {
    state.asInstanceOf[Flockers].params.asInstanceOf[Object]
  }
  
  override def start(){
    super.start()
    setupPortrayals()
  }
  
  override def load(state: SimState) {
    super.load(state)
    setupPortrayals()
  }
  
  def setupPortrayals() {
    val simulation = state.asInstanceOf[Flockers]

    setupVisitationRecordPortrayal(simulation)
    flockPortrayal.setField(simulation.flock)
    trailsPortrayal.setField(simulation.flock)
    
    for(flocker <- simulation.flock.allObjects) {
      val basic = new TrailedPortrayal2D(this, 
        new OrientedPortrayal2D(
          new SimplePortrayal2D(), 
          0, 
          4.0,
          randomColor(),
          OrientedPortrayal2D.SHAPE_COMPASS
        ),
        trailsPortrayal, 
        100
      )
      
      flockPortrayal.setPortrayalForObject(
        flocker,
        new AdjustablePortrayal2D(new MovablePortrayal2D(basic))
      )
      
      trailsPortrayal.setPortrayalForObject(flocker, basic)
    }

    val width = simulation.flock.getWidth
    val height = simulation.flock.getHeight
    if(width == height) {
      display.insideDisplay.width = 750.0
      display.insideDisplay.height = display.insideDisplay.width 
    }else if(width > height) {
      display.insideDisplay.width = 750.0
      display.insideDisplay.height = 750.0 * width / height
    }else if(width < height) {
      display.insideDisplay.width = 750.0 * height / width
      display.insideDisplay.height = 750.0
    }
    
    display.reset()
    display.repaint()
  }
  
  /**
   * Initiate the GUI.
   */
  override def init(controller: Controller) {
    super.init(controller)
    display = new Display2D(750, 750, this)
    display.setBackdrop(Color.black)
    
    displayFrame = display.createFrame()
    displayFrame.setTitle("Flockers")
    controller.registerFrame(displayFrame)
    displayFrame.setVisible(true)
    
    initVisitationRecordPortryal(display)
    display.attach(trailsPortrayal, "Contrails")
    display.attach(flockPortrayal, "The Flock")
  }
  
  
  /** Free up resources. */
  override def quit() {
    super.quit()
    
    if(displayFrame != null) { displayFrame.dispose() }
    displayFrame = null
    display = null
    
    // This is nessessary if you like hacking in SBT. If you don't include this
    // you will have to CTRL-C the simulation after every run command. 
    System.exit(0)
  }
  
  
  private def randomColor() = new Color(
    128 + guirandom.nextInt(128), 
    128 + guirandom.nextInt(128),
    128 + guirandom.nextInt(128)
  )
}

object FlockersWithUI {
  def main(args: Array[String]) { 
    new FlockersWithUI().createController() 
  }
  
  def getName(): String = "Flockers in Scala"
  
  /**
   * Scala's XML literals are a bit overkill, but it just saved a step in 
   * the interpolation of the resource icon path.
   */
  def getInfo(): Object = (
    <html>
      <img src={IconPath} />
      <h1>{Name}</h1>  
      <h2>{Author} (<a href={Email}>{Email}</a>)</h2>
      <h3><a href={Repository}>{Repository}</a></h3>
      <p>
        As part of MASON's examples, Sean Luke implemented a "Boids-style" flocking simulation. As part of my continuing evangelism of Scala as a language for Computational Social Sciences, I have ported Sean lukes code.
      </p>
      <p>
        To add a little pizazz, I created an visitationRecord field with a fast portrayal. This field turns the intuition that there may be dead spots and well traffic spots into an obvious graphical property. 
      </p>
      
    </html>
  ).toString
}
