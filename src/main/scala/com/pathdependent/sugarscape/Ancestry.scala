/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable

/**
 * Embues an agent with a sex.
 */
trait Ancestry extends Agent with DifferentiatedSexes {
  var mother: Option[AT] = None
  var father: Option[AT] = None
  var children = mutable.ListBuffer.empty[AT]
  
  /**
   * Gets the mother for GUI purposes.
   */
  def getMother(): Object = mother.getOrElse(null)
  
  /**
   * Gets the father for GUI purposes.
   */
  def getFather(): Object = father.getOrElse(null)
  
  /**
   * Gets the number of children for GUI purposes.
   */
  def getNumberOfChildren() = children.length
}

trait Reproduction extends Agent {
  def isFertile(): Boolean
}

trait GroupIdentity extends Agent {
  def getGroupIdentity: Int
}


