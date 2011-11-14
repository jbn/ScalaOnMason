/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.mason.examples

/**
 * This example is based on the MASON flockers example, written by Sean Luke.
 * It was ported line-for-line, then rewritten to reflect Scala's strengths.
 *
 * @see http://en.wikipedia.org/wiki/Boids
 * @see http://www.cs.gmu.edu/~eclab/projects/mason/
 
 * I've seperated the simulation into file names with clear meanings. Scala 
 * does not have one-file/one-class limits, nor does it require that the 
 * defined class is the same name as the file name. Obviously, it makes sense
 * to do so most of the time to avoid confusion; however, there can be
 * exceptions. In this case, most simulations consist of Agents, an Environment,
 * a Simulator, and a visualizer. MASON does not cleanly seperate the 
 * environment and the Simulator, and companion classes require same file
 * definitions, so I've reduced my convention to:
 *
 * <ul>
 *   <li>Agent.scala</li>
 *   <li>SimulationParameters.scala</li>
 *   <li>Simulation.scala</li>
 *   <li>SimulationWithGUI.scala</li>
 * </ul> 
 *
 * I think this will allow the reader to quickly orient themselves to my code.
 *
 * I also care much less about the speed of iterators; I use them to make code
 * less error prone.
 */
package object flockers {
  val Name       = "Flockers in Scala"
  val Author     = "John Bjorn Nelson"
  val Email      = "jbn@pathdependent.com"
  val IconPath   = getClass.getResource("/flockers/icon.png").toString
  val Repository = "https://github.com/jbn/ScalaOnMason"
}

