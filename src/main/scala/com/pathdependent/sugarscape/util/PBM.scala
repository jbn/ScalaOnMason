package com.pathdependent.sugarscape.util

import scala.util.parsing.combinator._

/**
 * NetPBM files are used for serialization and loading resource maps.
 *
 * They have the benefit of being easy to edit in something as simple as 
 * notepad and can be visualized with any PBM viewer. PBM is not an exact
 * fit, since it uses integers (technically one byte words) for each grayscale 
 * pixel. (This class only implements the 'P2' format.) Consequently, it can 
 * only load and save integers, whereas my implementation uses real numbers for 
 * most resource levels. However, it is sufficent for replicating existing work
 * on SugarScape, and that is my current goal.
 *
 * @see http://en.wikipedia.org/wiki/Netpbm_format
 */
class PBM(val pixels: Array[Array[Int]]) {
  val width  = pixels(0).length
  val height = pixels.length
  
  def minimumValue: Int = pixels.map(_.min).min
  
  def maximumValue: Int = pixels.map(_.max).max

  /**
   * Create a PBM of width x height with all pixels filled with 0 (black).
   */
  def this(width: Int, height: Int) = this(Array.fill[Int](width, height){ 0 })
  
  /**
   * @return pixel at (x,y)
   */
  def apply(x: Int, y: Int): Int = pixels(y)(x)
  
  /**
   * Sets pixel at (x,y) to value
   */
  def update(x: Int, y: Int, value: Int) = pixels(y)(x) = value

  /**
   * @return a PBM file as a string.
   */
  override def toString(): String = {
    "P2\n" + 
      "# See: http://en.wikipedia.org/wiki/Netpbm_format\n" + 
      width + " " + height + "\n" +
      maximumValue + "\n" +
      pixels.map(_.mkString(" ")).mkString("\n")
  }
}

/**
 * This companion object provides the parser for a PBM file. For such a 
 * simple format, it seems a bit overkill to use a parser combinator,
 * but it also seems less error prone. 
 */
object PBM extends JavaTokenParsers {
  def pbm: Parser[PBM] = "P2" ~> width ~ maxValue ~ pixels ^^ {
    case (x,y) ~ maxValue ~ pixels => {
      val expectedPixels = x * y
      
      if(expectedPixels != pixels.length) {
        throw new java.text.ParseException(
          "Expected " + expectedPixels + " pixels but found " + pixels.length,0
        )
      }
      
      new PBM(dividePixels(pixels, y))
    }
  }
  
  def width: Parser[(Int,Int)] = wholeNumber ~ wholeNumber ^^ { 
    case x ~ y => (x.toInt, y.toInt) 
  }
    
  def maxValue: Parser[Int] = wholeNumber ^^ (_.toInt)
  
  def pixels: Parser[List[Int]] = rep(wholeNumber) ^^ { l => l.map(_.toInt) }

  /**
   * Parses a PBM input string (format P2).
   *
   * @return the PBM object parsed from the input string
   *
   * @note I don't know how to strip comments within the parser combinator
   *       framework, so it is done manually, via String#replaceAll 
   *       prior to parseAll(). It's a bit silly to have a two-pass 
   *       parser on something that isn't even a recursive grammar!
   */
  def parse(input: String): PBM = {
    parseAll(pbm, input.replaceAll("""#.*""","")) match {
      case Success(pbm,_) => pbm
      case Failure(msg,_) => throw new java.text.ParseException(msg, 0)
    }
  }

  /**
   * Loads a url and parses it.
   */
  def parse(url: java.net.URL): PBM = {
    parse(scala.io.Source.fromURL(url).getLines.mkString("\n"))
  }
  
  /**
   * It's not tail recursive, but this method should only be called 
   * on initialization. I don't see it as any type of bottleneck.
   */
  def dividePixels(xs: List[Int], width: Int): Array[Array[Int]] = {
    val (rowAsList, rest) = xs.splitAt(width)
    val row = Array(rowAsList.toArray)

    if(rest.isEmpty) row else row ++ dividePixels(rest, width)
  }
}

