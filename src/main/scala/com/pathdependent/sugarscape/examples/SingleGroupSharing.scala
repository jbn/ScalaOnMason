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
import com.pathdependent.mason.ext.Helpers.{makeSteppable}




class SingleGroupSugarSharingAgent(
  @BeanProperty val depthOfVision: Int,
  @BeanProperty val basalSugarMetabolism: Double, 
  @BeanProperty val sugarEndowment: Double,
  @BeanProperty val sex: Sex,
  @BeanProperty val onsetAgeOfFertility: Int,
  @BeanProperty val terminalAgeOfFertility: Int,
  @BeanProperty val initialAge: Int,
  @BeanProperty val ageOfExpiration: Int,
  @BeanProperty val groupIdentity: Int,
  @BeanProperty val sugarSharingGenerosity: Double,
  @BeanProperty val fairnessEnforced: Boolean
) extends Agent
    with SugarConsumption with MovementRuleM 
    with SexRuleSWithAncestry
    with StaticGroupIdentity 
    with SugarSharing {
  type AT = SingleGroupSugarSharingAgent

  def spawnChild(rng: MersenneTwisterFast, partner: AT): AT = {
    // group identity is defined by the mother
    val m = if(sex == Female) this else partner
    val endowment = 0.5 * this.sugarEndowment + 0.5 * partner.sugarEndowment
    accumulatedSugar -= 0.5 * sugarEndowment
    partner.accumulatedSugar -= 0.5 * partner.sugarEndowment
    
    val childSex = if(rng.nextBoolean) Male else Female
    def randomParent() = if(rng.nextBoolean) this else partner
    
    val mutatedGenerosity = m.sugarSharingGenerosity + rng.nextDouble / 100.0 - 0.005
    
    new SingleGroupSugarSharingAgent(
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
      ageOfExpiration = randomParent.ageOfExpiration,
      groupIdentity = m.groupIdentity,
      sugarSharingGenerosity = (mutatedGenerosity max 0.0) min 1.0,
      fairnessEnforced = randomParent.fairnessEnforced
    )
  }
}


  
/**
 * I was unable to replicate the observed BZWave unless I the sugar
 * growback was severely retarded. I also think I probably need to turn off
 * the terroidal landscape, but this gives you nice "waves" at least.
 */
class SingleGroupSugarSharingSim(seed: Long) 
  extends Sugarscape(seed) 
    with SugarResources with TwoSugarMountains
    with SexRatio 
    with SnippedInheritance
    with MeanBasalSugarMetabolism
    with GroupOneRelativeDominance
    with MeanSugarSharingGenerosity {
  def this() = this(System.currentTimeMillis())
    
  type AT = SingleGroupSugarSharingAgent
  
  initialAgentDensity = 400.0 / (width * height)
  
  @BeanProperty var minDepthOfVision = 1
  @BeanProperty var maxDepthOfVision = 6

  @BeanProperty var inheritanceTransferLoss = 0.0
  def domInheritanceTransferLoss = new Interval(0.0, 1.0)

  @BeanProperty var shiftTerminalFertility = 0
  def domShiftTerminalFertility = new Interval(-25, 0)
  
  def getSugarWealth(): Double = {
    livingAgents.map { _.accumulatedSugar }.sum
  }
  
  def generateAgent(): SingleGroupSugarSharingAgent = {
    val basalSugarMetabolism = 1 + random.nextInt(4) // p.61.
    val sex = if(random.nextBoolean) Male else Female
    val groupID = random.nextInt(2)
    new SingleGroupSugarSharingAgent(
      depthOfVision = minDepthOfVision + random.nextInt(maxDepthOfVision - minDepthOfVision + 1),
      basalSugarMetabolism = basalSugarMetabolism,
      sugarEndowment = 50 + random.nextInt(51),
      sex = sex,
      onsetAgeOfFertility = 12 + random.nextInt(4),
      terminalAgeOfFertility = shiftTerminalFertility +
        40 + random.nextInt(11) + (if(sex == Male) 10 else 0),
      initialAge = random.nextInt(40),
      ageOfExpiration = 60 + random.nextInt(41),
      groupIdentity = 1,
      sugarSharingGenerosity = random.nextDouble,
      fairnessEnforced = random.nextBoolean
    )
  }
  
  def getAverageEndowment(): Double = {
    livingAgents.map(_.sugarEndowment).sum / livingAgents.length
  }
  override def toString = SingleGroupSugarSharingWithUI.getName()
}
  
class SingleGroupSugarSharingWithUI(
  rawState: SimState
) extends SugarscapeWithUI(rawState) 
    with SugarPortrayal with AgentPortrayal {
  def this() = this(new SingleGroupSugarSharingSim(System.currentTimeMillis))
  type ET = SingleGroupSugarSharingSim
}
  
object SingleGroupSugarSharingWithUI {
  def main(args: Array[String]) {
    (new SingleGroupSugarSharingWithUI()).createController() 
  }
    
  def getName(): String = "SugarScape SingleGroupSugarSharing"
    
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



