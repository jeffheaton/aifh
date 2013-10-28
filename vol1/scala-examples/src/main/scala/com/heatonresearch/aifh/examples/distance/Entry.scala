/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Scala Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.distance

import javax.swing._
import java.awt._
import java.awt.event.MouseEvent
import java.awt.image.PixelGrabber

/**
 * Entry: GUI element to allow the user to enter a character by drawing it.
 */
@SerialVersionUID(656936515012546346L)
class Entry extends JPanel {

  /**
   * constructor actions
   */
  enableEvents(AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK | AWTEvent.COMPONENT_EVENT_MASK)

  /**
   * Called to clear the image.
   */
  def clear() {
    entryGraphics.setColor(Color.white)
    entryGraphics.fillRect(0, 0, getWidth, getHeight)
    downSampleBottom = 0
    downSampleTop = 0
    downSampleLeft = 0
    downSampleRight = 0
    repaint()
  }

  /**
   * Called to downsample the image and store it in the down sample component.
   */
  def downSample() {
    val w: Int = entryImage.getWidth(this)
    val h: Int = entryImage.getHeight(this)
    val grabber: PixelGrabber = new PixelGrabber(entryImage, 0, 0, w, h, true)
    try {
      grabber.grabPixels
      pixelMap = grabber.getPixels.asInstanceOf[Array[Int]]
      findBounds(w, h)
      val data: SampleData = sample.data
      ratioX = (downSampleRight - downSampleLeft).toDouble / data.width.toDouble
      ratioY = (downSampleBottom - downSampleTop).toDouble / data.height.toDouble
      for(y <- 0 until data.height ;
          x <- 0 until data.width ) {
        data(x,y) = downSampleRegion(x, y)
      }
      sample.repaint()
      repaint()
    }
    catch {
      case e: InterruptedException =>
        e.printStackTrace()
    }
  }

  /**
   * Called to downsample a quadrant of the image.
   *
   * @param x The x coordinate of the resulting downsample.
   * @param y The y coordinate of the resulting downsample.
   * @return Returns true if there were ANY pixels in the specified quadrant.
   */
  protected def downSampleRegion(x: Int, y: Int): Boolean = {
    val w: Int = entryImage.getWidth(this)
    val startX: Int = (downSampleLeft + (x * ratioX)).toInt
    val startY: Int = (downSampleTop + (y * ratioY)).toInt
    val endX: Int = (startX + ratioX).toInt
    val endY: Int = (startY + ratioY).toInt

    for(yy <- startY to endY ;
        xx <- startX to endX) {
      val loc: Int = xx + (yy * w)
      if (pixelMap(loc) != -1) {
        return true
      }
    }
    false
  }

  /**
   * This method is called to automatically crop the image so that whitespace
   * is removed.
   *
   * @param w The width of the image.
   * @param h The height of the image
   */
  protected def findBounds(w: Int, h: Int) {
    val hNonEmptyLines = (0 until h) filter { y => hLineClear(y) }
    if(hNonEmptyLines.nonEmpty) {
      downSampleTop = hNonEmptyLines.head
      downSampleBottom = hNonEmptyLines.last
    }

    val vNonEmptyRows = (0 until w) filter { x => vLineClear(x) }
    if(vNonEmptyRows.nonEmpty) {
      downSampleLeft = vNonEmptyRows.head
      downSampleRight = vNonEmptyRows.last
    }
  }

  /**
   * This method is called internally to see if there are any pixels in the
   * given scan line. This method is used to perform autocropping.
   *
   * @param y The horizontal line to scan.
   * @return True if there were any pixels in this horizontal line.
   */
  protected def hLineClear(y: Int): Boolean = {
    val w: Int = entryImage.getWidth(this)
    for(i <- 0 until w) {
      if (pixelMap((y * w) + i) != -1) {
        return false
      }
    }
    true
  }

  /**
   * Setup the internal image that the user draws onto.
   */
  protected def initImage() {
    entryImage = createImage(getWidth, getHeight)
    entryGraphics = entryImage.getGraphics
    entryGraphics.setColor(Color.white)
    entryGraphics.fillRect(0, 0, getWidth, getHeight)
  }

  /**
   * Paint the drawn image and cropping box (if active).
   *
   * @param g The graphics context
   */
  override def paint(g: Graphics) {
    if (entryImage == null) {
      initImage()
    }
    g.drawImage(entryImage, 0, 0, this)
    g.setColor(Color.black)
    g.drawRect(0, 0, getWidth, getHeight)
    g.setColor(Color.red)
    g.drawRect(downSampleLeft, downSampleTop, downSampleRight - downSampleLeft, downSampleBottom - downSampleTop)
  }

  /**
   * Process messages.
   *
   * @param e The event.
   */
  protected override def processMouseEvent(e: MouseEvent) {
    if (e.getID != MouseEvent.MOUSE_PRESSED) {
      return
    }
    lastX = e.getX
    lastY = e.getY
  }

  /**
   * Process messages.
   *
   * @param e The event.
   */
  protected override def processMouseMotionEvent(e: MouseEvent) {
    if (e.getID != MouseEvent.MOUSE_DRAGGED) {
      return
    }
    entryGraphics.setColor(Color.black)
    entryGraphics.drawLine(lastX, lastY, e.getX, e.getY)
    getGraphics.drawImage(entryImage, 0, 0, this)
    lastX = e.getX
    lastY = e.getY
  }

  /**
   * This method is called to determine ....
   *
   * @param x The vertical line to scan.
   * @return True if there are any pixels in the specified vertical line.
   */
  protected def vLineClear(x: Int): Boolean = {
    val w: Int = entryImage.getWidth(this)
    val h: Int = entryImage.getHeight(this)
    for(i <- 0 until h) {
      if (pixelMap((i * w) + x) != -1) {
        return false
      }
    }
    true
  }

  /**
   * The image that the user is drawing into.
   */
  protected var entryImage: Image = null
  /**
   * A graphics handle to the image that the user is drawing into.
   */
  protected var entryGraphics: Graphics = null
  /**
   * The last x that the user was drawing at.
   */
  protected var lastX: Int = -1
  /**
   * The last y that the user was drawing at.
   */
  protected var lastY: Int = -1
  /**
   * The down sample component used with this component.
   */
  var sample: Sample = null
  /**
   * Specifies the left boundary of the cropping rectangle.
   */
  protected var downSampleLeft: Int = 0
  /**
   * Specifies the right boundary of the cropping rectangle.
   */
  protected var downSampleRight: Int = 0
  /**
   * Specifies the top boundary of the cropping rectangle.
   */
  protected var downSampleTop: Int = 0
  /**
   * Specifies the bottom boundary of the cropping rectangle.
   */
  protected var downSampleBottom: Int = 0
  /**
   * The downsample ratio for x.
   */
  protected var ratioX: Double = .0
  /**
   * The downsample ratio for y
   */
  protected var ratioY: Double = .0
  /**
   * The pixel map of what the user has drawn. Used to downsample it.
   */
  protected var pixelMap: Array[Int] = null
}
