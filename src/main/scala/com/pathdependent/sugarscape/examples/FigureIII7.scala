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

class FigureIII7Agent(
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
      with SexRuleS
      with SexRuleSWithAncestry {
  type AT = FigureIII7Agent
  
  def spawnChild(rng: MersenneTwisterFast, partner: AT): AT = {
    val endowment = 0.5 * this.sugarEndowment + 0.5 * partner.sugarEndowment
    accumulatedSugar -= 0.5 * sugarEndowment
    partner.accumulatedSugar -= 0.5 * partner.sugarEndowment
    
    val childSex = if(rng.nextBoolean) Male else Female
    
    def randomParent() = if(rng.nextBoolean) this else partner
    
    new FigureIII7Agent(
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
class FigureIII7Sim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains
    with PortionMale
    with SnippedInheritance {
  def this() = this(System.currentTimeMillis())
    
  type AT = FigureIII7Agent
  
  initialAgentDensity = 400.0 / (width * height)

  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6
  @BeanProperty var shiftTerminalFertility = 0
  def domShiftTerminalFertility = new Interval(-25, 0)
  
  @BeanProperty var inheritanceTransferLoss = 0.0
  def domInheritanceTransferLoss = new Interval(0.0,1.0)
  
  def getSugarWealth(): Double = {
    livingAgents.map { _.accumulatedSugar }.sum
  }
  
  def generateAgent(): FigureIII7Agent = {
    val basalSugarMetabolism = 1 + random.nextInt(4) // p.61.
    val sex = if(random.nextBoolean) Male else Female
    new FigureIII7Agent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = 50 + random.nextInt(51),
      sex = sex,
      onsetAgeOfFertility = 12 + random.nextInt(4),
      terminalAgeOfFertility = shiftTerminalFertility +
        40 + random.nextInt(11) + (if(sex == Male) 10 else 0),
      initialAge = random.nextInt(59),
      ageOfExpiration = 60 + random.nextInt(41)
    )
  }
  
  def getAverageEndowment(): Double = {
    livingAgents.map(_.sugarEndowment).sum / livingAgents.length
  }
  
  override def toString = FigureIII7WithUI.getName()
}
  
class FigureIII7WithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal with AgentPortrayal {
  def this() = this(new FigureIII7Sim(System.currentTimeMillis))
  type ET = FigureIII7Sim
}
  
object FigureIII7WithUI {
  def main(args: Array[String]) {
    (new FigureIII7WithUI()).createController() 
  }
    
  def getName(): String = "SugarScape FigureIII7"
    
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



