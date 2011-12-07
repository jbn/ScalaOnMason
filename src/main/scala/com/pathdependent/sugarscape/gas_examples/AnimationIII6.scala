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

import com.pathdependent.mason.ext.BinaryString

class AnimationIII6Agent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double,
  @BeanProperty val initialAge: Int,
  @BeanProperty val ageOfExpiration: Int,
  @BeanProperty val initialCulturalTags: BinaryString
) extends Agent
    with SugarConsumption 
      with MovementRuleM 
      with RuleK
      
  
class AnimationIII6Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains 
    with CulturalTagsDistribution
    with GroupOneRelativeDominance {
  def this() = this(System.currentTimeMillis())
    
  type AT = AnimationIII6Agent
  
  initialAgentDensity = 250.0 / (width * height)
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
  @BeanProperty var numberOfCulturalTags = 11
  
    
  def generateAgent(): AnimationIII6Agent = {
    new AnimationIII6Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = 1 + random.nextInt(6),
      sugarEndowment = 5 + random.nextInt(21),
      initialAge = 0,
      ageOfExpiration = 60 + random.nextInt(41),
      initialCulturalTags = BinaryString.generateRandom(
        random, numberOfCulturalTags
      )
    )
  }
  
  override def toString = AnimationIII6WithUI.getName()
}
  
class AnimationIII6WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal with AgentPortrayal {
  def this() = this(new AnimationIII6Sim(System.currentTimeMillis))
  type ET = AnimationIII6Sim
  
}
  
object AnimationIII6WithUI {
  def main(args: Array[String]) {
    (new AnimationIII6WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape AnimationIII6"
  
  
    
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



