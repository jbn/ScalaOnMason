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
import ec.util.MersenneTwisterFast

import com.pathdependent.mason.ext.PBM

class AnimationIII1Agent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double,
  @BeanProperty val sex: Sex,
  @BeanProperty val onsetAgeOfFertility: Int,
  @BeanProperty val terminalAgeOfFertility: Int,
  @BeanProperty val initialAge: Int,
  @BeanProperty val ageOfExpiration: Int
) extends Agent
    with SugarConsumption 
      with MovementRuleM 
      with SexRuleS {
  type AT = AnimationIII1Agent
  
  def spawnChild(rng: MersenneTwisterFast, partner: AT): AT = {
    val endowment = 0.5 * this.sugarEndowment + 0.5 * partner.sugarEndowment
    accumulatedSugar -= 0.5 * sugarEndowment
    partner.accumulatedSugar -= 0.5 * partner.sugarEndowment
    
    val childSex = if(rng.nextBoolean) Male else Female
    
    def randomParent() = if(rng.nextBoolean) this else partner
    
    new AnimationIII1Agent(
      depthOfVision = randomParent.depthOfVision,
      basalSugarMetabolism = randomParent.basalSugarMetabolism,
      sugarEndowment = endowment,
      sex = childSex,
      // Notice, this could be mendelian if we wanted.
      onsetAgeOfFertility = 12 + rng.nextInt(4), 
      terminalAgeOfFertility = 
        40 + rng.nextInt(11) + (if(childSex == Male) 10 else 0),
      // end Notice
      initialAge = 0,
      ageOfExpiration = randomParent.ageOfExpiration
    )
  }
}
  
/**
 * I was unable to replicate the observed BZWave unless I the sugar
 * growback was severely retarded. I also think I probably need to turn off
 * the terroidal landscape, but this gives you nice "waves" at least.
 */
class AnimationIII1Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains
    with PortionMale
    with MeanBasalSugarMetabolism {
  def this() = this(System.currentTimeMillis())
    
  type AT = AnimationIII1Agent
  
  initialAgentDensity = 400.0 / (width * height)
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
  @BeanProperty var shiftTerminalFertility = 0
  def domShiftTerminalFertility = new Interval(-25, 0)
  
  def generateAgent(): AnimationIII1Agent = {
    val basalSugarMetabolism = 1 + random.nextInt(6)
    val sex = if(random.nextBoolean) Male else Female
    new AnimationIII1Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = 50 + random.nextInt(51),
      sex = sex,
      onsetAgeOfFertility = 12 + random.nextInt(4),
      terminalAgeOfFertility = shiftTerminalFertility +
        40 + random.nextInt(11) + (if(sex == Male) 10 else 0),
      initialAge = random.nextInt(25),
      ageOfExpiration = 60 + random.nextInt(41)
    )
  }
  
  def getAverageEndowment(): Double = {
    livingAgents.map(_.sugarEndowment).sum / livingAgents.length
  }
  
  override def toString = AnimationIII1WithUI.getName()
}
  
class AnimationIII1WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal with AgentPortrayal {
  def this() = this(new AnimationIII1Sim(System.currentTimeMillis))
  type ET = AnimationIII1Sim
}
  
object AnimationIII1WithUI {
  def main(args: Array[String]) {
    (new AnimationIII1WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape AnimationIII1"
    
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



