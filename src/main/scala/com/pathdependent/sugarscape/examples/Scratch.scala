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

class ScratchAgent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double,
  @BeanProperty val basalSpiceMetabolism: Double,
  @BeanProperty val spiceEndowment: Double,
  @BeanProperty val sex: Sex
) extends Agent
    with SugarConsumption
    with SpiceConsumption with MovementRuleM
    with DifferentiatedSexes
    with Ancestry {
  type ET = ScratchSim
  var pollutionGeneratedBySugarExtraction = 1.0
}
  
class ScratchSim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains
    with SpiceResources with TwoSpiceMountains {
  def this() = this(System.currentTimeMillis())
    
  type AT = ScratchAgent
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
  @BeanProperty var diffusionInterval = 1
  @BeanProperty var pollutionGeneratedBySugarExtraction = 1.0
    
  def generateAgent(): ScratchAgent = {
    val basalSugarMetabolism = 1 + random.nextInt(6)
    val basalSpiceMetabolism = 1 + random.nextInt(6)
    val agent = new ScratchAgent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = basalSugarMetabolism,
      basalSpiceMetabolism = basalSpiceMetabolism,
      spiceEndowment = basalSpiceMetabolism,
      if(random.nextBoolean(0.5)) Male else Female
    )
    agent.mother = None
    agent
  }
  
  override def sugarGrowbackRule(location: Int2D, resource: Resource) {
    resource.capacityGrowback()
  }
  
  override def toString = ScratchWithUI.getName()
}
  
class ScratchWithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal {
  def this() = this(new ScratchSim(System.currentTimeMillis))
  type ET = ScratchSim
}
  
object ScratchWithUI {
  def main(args: Array[String]) {
    (new ScratchWithUI()).createController() 
  }
    
  def getName(): String = "SugarScape Scratch"
    
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



