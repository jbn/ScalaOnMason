/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

/** 
 * Provides methods for computing various statistics for inspection.
 *
 * It is convienent to place all the statistic generators in a single file,
 * this way the client can collect those relevant to his simulation.
 */
trait StatisticGenerator extends Sugarscape {
  def sum[T](seq: Iterable[T])(f: T => Double): Double = {
    seq.foldLeft(0.0){ (total: Double, x: T) => total + f(x) }
  }

  def portion[T](seq: Iterable[T])(predicate: T => Boolean): Double = {
    seq.count(predicate).toDouble / seq.size
  }

  def mean[T](seq: Iterable[T])(f: T => Double): Double = {
    sum(seq)(f) / seq.size
  }
  
}

trait MeanAgentDepthOfVision extends StatisticGenerator {
  type AT <: SugarConsumption
  
  def getMeanAgentDepthOfVision() = mean(livingAgents)(_.depthOfVision)
}

trait MeanAgentBasalSugarMetabolism extends StatisticGenerator {
  type AT <: SugarConsumption

  def getMeanAgentBasalSugarMetabolism(): Double = {
    mean(livingAgents)(_.basalSugarMetabolism)
  }
}

trait MeanSugarSharingGenerosity extends StatisticGenerator {
  type AT <: SugarSharing
  
  def getMeanSugarSharingGenerosity(): Double = {
    mean(livingAgents)(_.sugarSharingGenerosity)
  }
}

trait PortionEnforcingFairness extends StatisticGenerator {
  type AT <: SugarSharing
  
  def getPortionEnforcingFairness() = portion(livingAgents)(_.fairnessEnforced)
}

trait PortionFertile extends StatisticGenerator {
  type AT <: SexualReproduction
  
  def getPortionFertile() = portion(livingAgents)(_.isFertile)
}

trait PortionMale extends StatisticGenerator {
  type AT <: DifferentiatedSexes 
  
  def getPortionMale(): Double = portion(livingAgents)(_.sex.isMale)
}

trait CulturalTagsDistribution extends Sugarscape {
  type AT <: Culture 


  def getCulturalTagsDistribution(): Array[Int] = {
    val numberLiving = livingAgents.length
    
    if(numberLiving > 0) {
      val n = livingAgents.head.culturalTags.length
      val tagCounts = new Array[Int](n)
      val range = (0 until n)
      livingAgents.foreach { agent => 
        range.foreach { i => if(agent.culturalTags(i)) tagCounts(i) += 1 }
      }
      
      val tagDistribution = new Array[Int](tagCounts.sum)
      var i = 0
      tagCounts.zipWithIndex.foreach { case (count, tag) =>
        (1 to count).foreach { _ => tagDistribution(i) = tag + 1; i += 1 }
      }
      tagDistribution
    } else {
      new Array[Int](0)
    }
  }
}

trait AgeDistribution extends Sugarscape {
  type AT <: FiniteLifespan
  
  def getAgeDistribution(): Array[Int] = livingAgents.map(_.age).toArray
}

trait GroupOneRelativeDominance extends Sugarscape {
  type AT <: GroupIdentity

  def getPercentInGroupOne(): Double = {
    val numberLiving = livingAgents.length
    
    if(numberLiving > 0) {
      var numberInGroup1 = 0

      livingAgents.foreach { 
        agent => if(agent.groupIdentity == 1) numberInGroup1 += 1 
      }

      numberInGroup1.toDouble / numberLiving * 100.0
    } else {
      -1.0
    }
  }
}



