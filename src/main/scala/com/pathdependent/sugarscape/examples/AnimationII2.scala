/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape.examples

import com.pathdependent.sugarscape._
import sim.engine.SimState
import sim.util.Int2D

class AnimationII2Sim(seed: Long) extends AnimationII1Sim(seed) {
  def this() = this(System.currentTimeMillis())
  
  /**
   * Yea..This is overridden twice...
   *
   * @todo make AnimationII1 extend AnimationII2...which is weird.
   */
  override def sugarGrowbackRule(location: Int2D, resource: Resource) {
    resource.unitGrowback()
  }
}

class AnimationII2WithUI(rawState: SimState) 
    extends AnimationII1WithUI(rawState){
  def this() = this(new AnimationII2Sim(System.currentTimeMillis))
}

object AnimationII2WithUI {
  def main(args: Array[String]) {
    (new AnimationII2WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape in Scala on Mason"
    
  def getInfo(): Object = (
    <html>
      <img src={IconPath} />
      <h1>{Name}</h1>  
      <h2>{Author} (<a href={Email}>{Email}</a>)</h2>
      <h3><a href={Repository}>{Repository}</a></h3>
      <p>
        I'm really leaning on Scala's type system.
      </p>
       
    </html>
  ).toString
}


