/*
   ____   ___   __   __     __      __   __ _    _  _   __   ____   __   __ _ 
  / ___) / __) / _\ (  )   / _\    /  \ (  ( \  ( \/ ) / _\ / ___) /  \ (  ( \
  \___ \( (__ /    \/ (_/\/    \  (  O )/    /  / \/ \/    \\___ \(  O )/    /
  (____/ \___)\_/\_/\____/\_/\_/   \__/ \_)__)  \_)(_/\_/\_/(____/ \__/ \_)__)

         Copyright John Bjorn Nelson <jbn@pathdependent.com>, 2011.

*/
package com.pathdependent.sugarscape

import scala.collection.mutable.HashSet

trait Kinship extends Agent {
  var kinshipGrammar: KinshipGrammar
}

abstract class KinshipGrammar {
  val knownKin = HashSet.empty[Object]
  
  def identifyKin(origin: Ancestry): HashSet[Object]
  def isKinOf(other: Ancestry) = knownKin contains other
}

class DegreesOfSeperationKinship(val k: Int) extends KinshipGrammar {
  def identifyKin(origin: Ancestry): HashSet[Object] = {
    def collectKin(myObj: Object, i: Int): Unit = if(i < k){
      val my = myObj.asInstanceOf[Ancestry]
      val immediateKin = HashSet.empty[Object]
      
      def addWithChildren(obj: Object) {
        knownKin += obj
        immediateKin += obj
        obj.asInstanceOf[Ancestry].children.foreach { sibling =>
          knownKin += sibling
          immediateKin += sibling
        }
      }
      
      my.mother.foreach(addWithChildren)
      my.father.foreach(addWithChildren)
      
      knownKin ++= my.children
      immediateKin ++= my.children
      
      immediateKin.foreach(collectKin(_, i+1))
    }
    
    collectKin(origin, 0)
    
    knownKin
  }
}
