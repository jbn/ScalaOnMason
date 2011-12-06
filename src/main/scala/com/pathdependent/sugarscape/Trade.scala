/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable.ListBuffer
import scala.math.{sqrt,pow,log}

import com.pathdependent.mason.ext.Helpers.makeSteppable

trait TradeRecorder extends Sugarscape {
  var tradeVolume = 0
  def getTradeVolume = tradeVolume
  var meanPrice = 0.0
  def getMeanPrice() = meanPrice
  var stdDevPrice = 0.0
  def getStdDevPrice = stdDevPrice
  
  val tradeBuffer = ListBuffer.empty[Double]
  
  def recordTrade(price: Double) {
    tradeBuffer += price
  }
  
  /**
   * Initializes the landscape of spice. Schedules growback.
   */
  override def fieldInitializerChain() {
    super.fieldInitializerChain()
    
    tradeVolume = 0
    meanPrice = 0.0
    stdDevPrice = 0.0    
        
    schedule.scheduleRepeating(
      1.0, Sugarscape.Ordering.Statistics,
      makeSteppable[TradeRecorder] { tradeRecorder =>
        if(tradeRecorder.tradeBuffer.length == 0) {
          tradeRecorder.tradeVolume = 0
          tradeRecorder.meanPrice = 0
          tradeRecorder.stdDevPrice = 0
        } else {
          tradeRecorder.tradeVolume = tradeRecorder.tradeBuffer.length
          tradeRecorder.meanPrice = tradeRecorder.tradeBuffer.map(log).sum / tradeRecorder.tradeBuffer.length
          tradeRecorder.stdDevPrice = sqrt(
            tradeRecorder.tradeBuffer.
              map { price => pow(log(price) - tradeRecorder.meanPrice, 2.0) }.
              sum / tradeRecorder.tradeBuffer.length
          )
          tradeRecorder.tradeBuffer.clear
        }
      }
    )
  }
}

trait TradeRuleT extends MultiCommodityMovementRule {
  type AT <: MultiCommodityMovementRule
  type ET <: TradeRecorder with SugarResources with SpiceResources
  
  override def interact(sugarscape: ET) { 
    tradeRuleT(sugarscape)
    
    super.interact(sugarscape)
  }
  
  /**
   * This is the agent trade rule, T. The following is taken directly from
   * GAS.
   *
   * <ul>
   *   <li>Agent and neighbor compute their MRSs; if these are equal then
   *       end, else continue; </li>
   *   <li>The direction of exchange is as follows: spice flows from the
   *       agent with the higher MRS to the agent with the lower MRS
   *       while sugar goes in the opposite direction;</li>
   *   <li>The geometric mean of the two MRSs is calculated -- this will serve
   *       as the price, p;</li>
   *   <li>The quantities to be exchanged are as follows: if p > 1 then p 
   *       units of spice for 1 unit of sugar; if p < 1 then 1 / p units of 
   *       sugar for 1 unit of spice</li>
   *   <li>If this trade will (a) make both agents better off (increases the
   *       welfare of both agents), and (b) not cause the agents' MRSs to 
   *       cross over one another, then the trade is made and return to start, 
   *       else end.</li>
   * </ul>
   *
   * @see Growing Artificial Socieites, p.105.
   */
  def tradeRuleT(sugarscape: ET) = {
    def sign(a: Double, b: Double): Int = if(a > b) 1 else -1
    
    def trade(partner: AT) {
      val myMRS = marginalRateOfSubstitution(accumulatedSugar, accumulatedSpice)

      val theirMRS = marginalRateOfSubstitution(
        partner.accumulatedSugar, partner.accumulatedSpice
      )
      
      val myWelfare = welfare(accumulatedSugar, accumulatedSpice)
      val theirWelfare = welfare(partner.accumulatedSugar, partner.accumulatedSpice)
      
      if(myMRS != theirMRS) { 
        // spice goes from high MRS -> lowMRS
        val price = sqrt(myMRS * theirMRS)
        
        
        if(myMRS > theirMRS) {
          val myNewMRS = marginalRateOfSubstitution(
            accumulatedSugar + 1.0, accumulatedSpice - price
          )
          val theirNewMRS = marginalRateOfSubstitution(
            partner.accumulatedSugar - 1.0, partner.accumulatedSpice + price
          )
          val myNewWelfare = welfare(accumulatedSugar + 1.0, accumulatedSpice - price)
          val theirNewWelfare = welfare(
            partner.accumulatedSugar - 1.0, partner.accumulatedSpice + price
          )
          
          if(myNewWelfare >= myWelfare && theirNewWelfare >= myNewWelfare &&
             (myNewMRS == theirNewMRS || 
              sign(myMRS, theirMRS) == sign(myNewMRS, theirNewMRS))) {
            accumulatedSugar += 1
            accumulatedSpice -= price
            partner.accumulatedSugar -= 1
            partner.accumulatedSpice += price
            
            sugarscape.recordTrade(price)
            trade(partner)
          }
        } else {
          val myNewMRS = marginalRateOfSubstitution(
            accumulatedSugar - 1.0 / price, accumulatedSpice + 1
          )
          val theirNewMRS = marginalRateOfSubstitution(
            partner.accumulatedSugar + 1.0 / price, partner.accumulatedSpice - 1
          )
          val myNewWelfare = welfare(accumulatedSugar - 1.0 / price, accumulatedSpice + 1)
          val theirNewWelfare = welfare(
            partner.accumulatedSugar + 1.0 / price, partner.accumulatedSpice - 1
          )

          
          if(myNewWelfare >= myWelfare && theirNewWelfare >= myNewWelfare &&
             (myNewMRS == theirNewMRS || 
              sign(myMRS, theirMRS) == sign(myNewMRS, theirNewMRS))) {
            accumulatedSugar -= 1.0 / price
            accumulatedSpice += 1.0
            partner.accumulatedSugar += 1.0 / price
            partner.accumulatedSpice -= 1.0
            
            sugarscape.recordTrade(price)
            trade(partner)
          }
        }
      }
    }
    
    val tradePartners = immediateNeighborhood(sugarscape.random).
      map { loc => sugarscape.agentAt(sugarscape.translateLocation(loc))}.
      filterNot( _ == null)

    tradePartners.foreach{ agent =>trade(agent.asInstanceOf[AT]) }
  }  
}
