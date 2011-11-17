/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.ext

import java.awt.{Graphics2D, Paint}
import sim.portrayal.DrawInfo2D
import sim.portrayal.simple.OvalPortrayal2D

/**
 * This portrayal is an OvalPortrayal with a variable scale. 
 *
 * It is an imitation of the animations produced in Growing Artificial
 * societies, where the circular portrayal of Sugar was a function of the 
 * level of sugar at a particular location.
 */
abstract class BallooningPortrayal(
  color: Paint, isFilled: Boolean = true
) extends OvalPortrayal2D(color, 0.0, isFilled) {
  override def draw(obj: Object, graphics: Graphics2D, drawInfo: DrawInfo2D) {
    scale = scaleExtractor(obj)
    super.draw(obj, graphics, drawInfo)
  }

  /** 
   * @return a value between 0 and 1, representing the radius of the oval
   *         to be drawn.
   */  
  def scaleExtractor(obj: Object): Double
}

object BallooningPortrayal {
  /**
   * A convience factory method. Does the casting and builds the anonymous
   * class for you. You only have to provide a functional object that
   * maps an object to a double.
   *
   * @note if you don't make the calling functional object's arguments typed,
   *       you'll get a counter-intuitive compiler error about "Nothing".
   */
  def apply[T](color: Paint, isFilled: Boolean = true)
      (f: (T) => Double): BallooningPortrayal = {
    new BallooningPortrayal(color, isFilled) {
      def scaleExtractor(obj: Object): Double = f(obj.asInstanceOf[T])
    }  
  }
}

