/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape.examples

import com.pathdependent.sugarscape._

import scala.reflect.{BeanProperty}

import sim.engine.{SimState}
import sim.util.Int2D

class AnimationII1Agent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double
) extends Agent
    with SugarConsumption 
      with MovementRuleM 
  
class AnimationII1Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains{
  def this() = this(System.currentTimeMillis())
    
  type AT = AnimationII1Agent
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
    
  def generateAgent(): AnimationII1Agent = {
    val basalSugarMetabolism = 1 + random.nextInt(6)
    new AnimationII1Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = basalSugarMetabolism
    )
  }
  
  override def sugarGrowbackRule(location: Int2D, resource: Resource) {
    resource.capacityGrowback()
  }
  
  override def toString = AnimationII1WithUI.getName()
}
  
class AnimationII1WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal {
  def this() = this(new AnimationII1Sim(System.currentTimeMillis))
  type ET = AnimationII1Sim
}
  
object AnimationII1WithUI {
  def main(args: Array[String]) {
    (new AnimationII1WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape AnimationII1"
    
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



