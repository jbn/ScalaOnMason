/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape.examples

import scala.reflect.BeanProperty

import com.pathdependent.sugarscape._
import sim.engine.SimState
import sim.util.{Int2D, Interval}

class AnimationII7(seed: Long) 
  extends AnimationII1Sim(seed) with SeasonalSugar {
  
  def this() = this(System.currentTimeMillis())
  
  @BeanProperty var durationOfSugarSeason = 50
  @BeanProperty var winterSugarGrowbackRate = 1.0 / 8.0
  def domWinterSugarGrowbackRate() = new Interval(0.0, 1.0)
  @BeanProperty var summerSugarGrowbackRate = 1.0
  def domSummerSugarGrowbackRate() = new Interval(0.0, 1.0)
}

class AnimationII7WithUI(rawState: SimState) 
    extends AnimationII1WithUI(rawState){
  def this() = this(new AnimationII7(System.currentTimeMillis))
}

object AnimationII7WithUI {
  def main(args: Array[String]) {
    (new AnimationII7WithUI()).createController() 
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


