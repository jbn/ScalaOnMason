/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import javax.swing.JFrame
import java.awt.{Color}

import sim.engine.{SimState}
import sim.display.{Display2D,GUIState,Controller}
import sim.portrayal.Inspector

class SugarscapeWithUI(rawState: SimState) extends GUIState(rawState){
  type ST <: Sugarscape
  
  /** Show the Parameters object, not this. */
  override def getSimulationInspectedObject() = state
  
  var display: Display2D = null
  var displayFrame: JFrame = null
  
  override def getInspector(): Inspector = {
    val inspector = super.getInspector()
    inspector.setVolatile(true)
    inspector
  }
  
  override def start() {
    super.start()
    setupPortrayals()
  }
  
  override def load(recoveredState: SimState) {
    super.load(recoveredState)
    setupPortrayals()
  }
  
  def setupPortrayals() {
    val sugarscape = state.asInstanceOf[ST]
    
    setupPortrayal(sugarscape)
  
    val width = sugarscape.width
    val height = sugarscape.height
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
   * Chained hook for setting up a field portrayal.
   */
  def setupPortrayal(sugarscape: ST) {}
  
  /**
   * Chained hook for setting up the display. (Mostly, attaching portrayals.)
   */
  def setupDisplay(display: Display2D) {}
  
  /**
   * Initiate the GUI.
   */
  override def init(controller: Controller) {
    super.init(controller)
    
    display = new Display2D(750, 750, this)
    display.setBackdrop(Color.black)
    
    displayFrame = display.createFrame()
    displayFrame.setTitle("SugarScape")
    controller.registerFrame(displayFrame)
    displayFrame.setVisible(true)
    
    display.insideDisplay.setupHints(true, true, true)
    
    setupDisplay(display)
  }
  
  
  
  override def quit() {
    super.quit()
    
    if(displayFrame != null) { displayFrame.dispose() }
    displayFrame = null
    display = null
    
    // This is nessessary if you like hacking in SBT. If you don't include this
    // you will have to CTRL-C the simulation after every run command. 
    System.exit(0)
  }
}

