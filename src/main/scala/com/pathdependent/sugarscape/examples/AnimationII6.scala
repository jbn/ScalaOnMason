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
import sim.util.{Int2D, Interval}

import com.pathdependent.mason.ext.PBM

class AnimationII6Agent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double
) extends Agent
    with SugarConsumption 
      with MovementRuleM 
  
/**
 * I was unable to replicate the observed BZWave unless I the sugar
 * growback was severely retarded. I also think I probably need to turn off
 * the terroidal landscape, but this gives you nice "waves" at least.
 */
class AnimationII6Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains{
  def this() = this(System.currentTimeMillis())
    
  type AT = AnimationII6Agent
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 10
  
  def generateAgent(): AnimationII6Agent = {
    val basalSugarMetabolism = 1 + random.nextInt(6)
    new AnimationII6Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = basalSugarMetabolism * 10
    )
  }
  
  override def generateInitialAgentLocations(): List[Int2D] = {
    val initialLocations = PBM.parse(
      getClass.getResource("/sugarscape/AgentWave.pbm")
    )
    
    allLocations.filter { loc => initialLocations(loc.x, loc.y) == 1 }
  }
  
  override def sugarGrowbackRule(location: Int2D, resource: Resource) {
    resource.exponentialGrowback(0.1, 0.2)
  }
  
  override def toString = AnimationII6WithUI.getName()
}
  
class AnimationII6WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal 
    with AgentPortrayal {
  def this() = this(new AnimationII6Sim(System.currentTimeMillis))
  type ET = AnimationII6Sim
}
  
object AnimationII6WithUI {
  def main(args: Array[String]) {
    (new AnimationII6WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape AnimationII6"
    
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



