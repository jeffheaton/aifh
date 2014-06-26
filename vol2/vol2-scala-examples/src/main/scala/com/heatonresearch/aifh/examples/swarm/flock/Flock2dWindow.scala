/*
* Artificial Intelligence for Humans
* Volume 2: Nature Inspired Algorithms
* Java Version
* http://www.aifh.org
* http://www.jeffheaton.com
*
* Code repository:
* https://github.com/jeffheaton/aifh
*
* Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.examples.swarm.flock

import com.heatonresearch.aifh.distance.CalculateDistance
import com.heatonresearch.aifh.distance.EuclideanDistance
import javax.swing._
import java.awt._
import java.awt.event.ComponentEvent
import java.awt.event.ComponentListener
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.awt.image.BufferedImage

/**
 * This example plots a flock of particles that exhibit flocking behavior.  This is governed by the following three
 * simple rules:
 * <p/>
 * 1. Separation - avoid crowding neighbors (short range repulsion)
 * 2. Alignment - steer towards average heading of neighbors
 * 3. Cohesion - steer towards average position of neighbors (long range attraction)
 * <p/>
 * References:
 * <p/>
 * http://en.wikipedia.org/wiki/Flocking_(behavior)
 */
object Flock2dWindow {
  /**
   * Main entry point.
   *
   * @param args Not used.
   */
  def main(args: Array[String]) {
    val app: Flock2dWindow = new Flock2dWindow
    app.setVisible(true)
  }

  /**
   * Find the index that has the max value in a vector.
   *
   * @param data The vector.
   * @return The index.
   */
  def maxIndex(data: Array[Double]): Int = {
    var result: Int = -1
    for(i <- 0 until data.length) {
      if (result == -1 || data(i) > data(result)) {
        result = i
      }
    }
    result
  }

  /**
   * Get the mean particle location for the specified dimension.
   *
   * @param particles The particles.
   * @param dimension The dimension.
   * @return The mean.
   */
  def particleLocationMean(particles: Iterable[Particle], dimension: Int): Double = {
    var sum = 0.0
    var count: Int = 0
    for (p <- particles) {
      sum += p.location(dimension)
      count += 1
    }
    sum / count
  }

  /**
   * Get the particle velocity mean for the specified dimension.
   *
   * @param particles The particles.
   * @param dimension The dimension.
   * @return The velocity mean.
   */
  def particleVelocityMean(particles: Iterable[Particle], dimension: Int): Double = {
    var sum: Double = 0
    var count: Int = 0
    for (p <- particles) {
      sum += p.velocity(dimension)
      count += 1
    }
    sum / count
  }

  /**
   * The number of particles.
   */
  private val PARTICLE_COUNT: Int = 100
  /**
   * The size of each particle.
   */
  private val PARTICLE_SIZE: Double = 10
  /**
   * The constant for cohesion.
   */
  private val constCohesion: Double = 0.01
  /**
   * The constant for alignment.
   */
  private val constAlignment: Double = 0.5
  /**
   * The constant for separation.
   */
  private val constSeparation: Double = 0.25
}

class Flock2dWindow extends JFrame with Runnable with ComponentListener with WindowListener {
  import Flock2dWindow._

  /**
   * An off-screen image to render to.
   */
  private var offscreenGraphics: Graphics = null
  /**
   * The off screen image.
   */
  private var offscreenImage: BufferedImage = null
  /**
   * Distance calculation.
   */
  private val distanceCalc: CalculateDistance = new EuclideanDistance

  setTitle("Flocking in 2D")

  setSize(1024, 768)

  /**
   * The particles.
   */
  private val particles: Iterable[Particle] = for(i <- 0 until PARTICLE_COUNT) yield {
    val location = Array(Math.random * this.getWidth,Math.random * this.getHeight)
    val velocity = Array(3.0,Math.random * 2.0 * Math.PI)
    new Particle(location,velocity)
  }

  this.addWindowListener(this)
  this.addComponentListener(this)

  // end of construction

  /**
   * Find the nearest neighbor particle.
   *
   * @param target    The particle to look for neighbors to.
   * @param particles All particles.
   * @param k         The number of particles to find.
   * @param maxDist   The max distance to check.
   * @return The nearest neighbors.
   */
  private def findNearest(target: Particle, particles: Iterable[Particle], k: Int,
                          maxDist: Double): Iterable[Particle] = {
    val result: java.util.List[Particle] = new java.util.ArrayList[Particle]
    val tempDist: Array[Double] = new Array[Double](k)
    var worstIndex: Int = -1
    for (particle <- particles) {
      if (!(particle eq target)) {
        val d = distanceCalc.calculate(particle.location, target.location)
        if (d <= maxDist) {
          if (result.size < k) {
            tempDist(result.size) = d
            result.add(particle)
            worstIndex = maxIndex(tempDist)
          }
          else if (d < tempDist(worstIndex)) {
            tempDist(worstIndex) = d
            result.set(worstIndex, particle)
            worstIndex = maxIndex(tempDist)
          }
        }
      }
    }
    import scala.collection.JavaConversions._
    result
  }

  /**
   * Perform the flocking.
   */
  private def flock() {
    for (particle <- this.particles) {
      val neighbors = findNearest(particle, this.particles, 5, Double.PositiveInfinity)
      val nearest = findNearest(particle, this.particles, 5, 10)
      var separation: Double = 0
      if (nearest.size > 0) {
        val meanX = particleLocationMean(nearest, 0)
        val meanY = particleLocationMean(nearest, 1)
        val dx = meanX - particle.location(0)
        val dy = meanY - particle.location(1)
        separation = Math.atan2(dx, dy) - particle.velocity(1)
        separation += Math.PI
      }
      var alignment: Double = 0
      if (neighbors.size > 0) {
        alignment = particleVelocityMean(neighbors, 1) - particle.velocity(1)
      }
      var cohesion: Double = 0
      if (neighbors.size > 0) {
        val meanX = particleLocationMean(this.particles, 0)
        val meanY = particleLocationMean(this.particles, 1)
        val dx = meanX - particle.location(0)
        val dy = meanY - particle.location(1)
        cohesion = Math.atan2(dx, dy) - particle.velocity(1)
      }
      val turnAmount = (cohesion * constCohesion) + (alignment * constAlignment) + (separation * constSeparation)
      particle.velocity(1) += turnAmount
    }
  }

  override def run() {
    this.offscreenImage = new BufferedImage(getWidth, getHeight, BufferedImage.TYPE_INT_ARGB)
    this.offscreenGraphics = this.offscreenImage.createGraphics
    while (true) {
      this.offscreenGraphics.setColor(Color.black)
      this.offscreenGraphics.fillRect(0, 0, getWidth, getHeight)
      val x = new Array[Int](3)
      val y = new Array[Int](3)
      this.offscreenGraphics.setColor(Color.white)
      for (p <- this.particles) {
        x(0) = p.location(0).asInstanceOf[Int]
        y(0) = p.location(1).asInstanceOf[Int]
        val r = p.velocity(1) + (Math.PI * 5.0) / 12.0
        x(1) = x(0) - (Math.cos(r) * PARTICLE_SIZE).asInstanceOf[Int]
        y(1) = y(0) - (Math.sin(r) * PARTICLE_SIZE).asInstanceOf[Int]
        val r2 = p.velocity(1) + (Math.PI * 7.0) / 12.0
        x(2) = x(0) - (Math.cos(r2) * PARTICLE_SIZE).asInstanceOf[Int]
        y(2) = y(0) - (Math.sin(r2) * PARTICLE_SIZE).asInstanceOf[Int]
        this.offscreenGraphics.drawPolygon(x, y, 3)
        val dx = Math.cos(r)
        val dy = Math.sin(r)
        p.location(0) += (dx * p.velocity(0))
        p.location(1) += (dy * p.velocity(0))
        if (p.location(0) < 0) {
          p.location(0) = getWidth
        }
        if (p.location(1) < 0)
          p.location(1) = getHeight

        if (p.location(0) > getWidth)
          p.location(0) = 0

        if (p.location(1) > getHeight)
          p.location(1) = 0

      }
      flock()
      try {
        Thread.sleep(10)
      } catch {
        case e: InterruptedException =>
          e.printStackTrace()
      }
      val g = this.getGraphics
      g.drawImage(this.offscreenImage, 0, 0, this)
    }
  }

  override def componentResized(e: ComponentEvent) {
    this.offscreenImage = new BufferedImage(getWidth, getHeight, BufferedImage.TYPE_INT_ARGB)
    this.offscreenGraphics = this.offscreenImage.getGraphics
  }

  override def componentMoved(e: ComponentEvent) {}

  override def componentShown(e: ComponentEvent) {}

  override def componentHidden(e: ComponentEvent) {}

  def windowOpened(e: WindowEvent) {
    val t = new Thread(this)
    t.start()
  }

  override def windowClosing(e: WindowEvent) {}

  override def windowClosed(e: WindowEvent) {}

  override def windowIconified(e: WindowEvent) {}

  override def windowDeiconified(e: WindowEvent) {}

  override def windowActivated(e: WindowEvent) {}

  override def windowDeactivated(e: WindowEvent) {}

}