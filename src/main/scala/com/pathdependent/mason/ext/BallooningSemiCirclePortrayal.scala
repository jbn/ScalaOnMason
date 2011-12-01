/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.ext

import java.awt.{Color,Graphics2D,Paint}
import java.awt.geom.Arc2D
import sim.portrayal.DrawInfo2D
import sim.portrayal.simple.OvalPortrayal2D

/**
 * @note I'm not sure how Copyrights work in AFL. I'm just stealing 
 *       Sean Luke's code.
 *
 * @see sim/portrayal/simple/OvalPortrayal2D.java
 */
abstract class BallooningSemiCirclePortrayal(
  startAngle: Int, 
  angleExtent: Int,
  color: Paint, 
  isFilled: Boolean = true
) extends OvalPortrayal2D(color, 0.0, isFilled) {
  @transient var preciseSemiCircle = new Arc2D.Double

  override def draw(obj: Object, graphics: Graphics2D, drawInfo: DrawInfo2D) {
    scale = scaleExtractor(obj)
    
    val draw = drawInfo.draw
    val width = draw.width * scale + offset
    val height = draw.height * scale + offset
    
    graphics.setPaint(paint)
    
    if(drawInfo.precise) {
      if(preciseSemiCircle == null){
        preciseSemiCircle = new Arc2D.Double
      }
      preciseSemiCircle.setAngleStart(startAngle)
      preciseSemiCircle.setAngleExtent(angleExtent)
      preciseSemiCircle.setFrame(
        drawInfo.draw.x - width / 2.0, drawInfo.draw.y - height / 2.0, 
        width, height
      )
      if(filled) {
        graphics.fill(preciseSemiCircle)
      } else {
        graphics.draw(preciseSemiCircle)
      }
    } else {
      val x = (draw.x - width / 2.0).toInt
      val y = (draw.y - height / 2.0).toInt
      val w = width.toInt
      val h = height.toInt
      
      if(filled) {
        graphics.fillArc(x, y, w, h, startAngle, angleExtent)
      } else {
        graphics.drawArc(x, y, w, h, startAngle, angleExtent)
      }
    }
    
  }

  /** 
   * @return a value between 0 and 1, representing the radius of the oval
   *         to be drawn.
   */  
  def scaleExtractor(obj: Object): Double
}

object BallooningSemiCirclePortrayal{
  def apply[T](startAngle: Int, angleExtent: Int,color: Paint, 
      isFilled: Boolean = true) 
      (f: (T) => Double): BallooningSemiCirclePortrayal = {
    new BallooningSemiCirclePortrayal(startAngle, angleExtent, color, isFilled) {
      def scaleExtractor(obj: Object): Double = f(obj.asInstanceOf[T])
    }  
  }
}
