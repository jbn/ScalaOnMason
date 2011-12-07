/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape.gas_examples

import com.pathdependent.sugarscape._

import scala.reflect.{BeanProperty}

import sim.engine.{SimState}
import sim.util.Int2D

class AnimationII3Agent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double,
  @BeanProperty val initialAge: Int,
  @BeanProperty val ageOfExpiration: Int
) extends Agent
    with SugarConsumption 
      with MovementRuleM 
      with FiniteLifespan
  
class AnimationII3Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains
    with DeferredReplacement 
    with SugarResourcesDistribution
    with AgeDistribution {
  def this() = this(System.currentTimeMillis())
    
  type AT = AnimationII3Agent
  
  initialAgentDensity = 250.0 / (width * height)
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
    
  def generateAgent(): AnimationII3Agent = {
    new AnimationII3Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = 1 + random.nextInt(6),
      sugarEndowment = 5 + random.nextInt(21),
      initialAge = 0,
      ageOfExpiration = 60 + random.nextInt(41)
    )
  }
  
  override def sugarGrowbackRule(location: Int2D, resource: Resource) {
    resource.unitGrowback()
  }

  override def toString = AnimationII3WithUI.getName()
}
  
class AnimationII3WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal with AgentPortrayal{
  def this() = this(new AnimationII3Sim(System.currentTimeMillis))
  type ET = AnimationII3Sim
  
}
  
object AnimationII3WithUI {
  def main(args: Array[String]) {
    (new AnimationII3WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape AnimationII3"
  
  
    
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



