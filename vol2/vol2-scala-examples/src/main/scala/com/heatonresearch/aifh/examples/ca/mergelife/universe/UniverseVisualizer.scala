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
package com.heatonresearch.aifh.examples.ca.mergelife.universe

import java.awt._
import java.awt.image.BufferedImage
import java.awt.image.WritableRaster

/**
 * Visualize the universe.
 *
 * @param universe The universe.
 * @param zoom     The zoom factor.
 */
class UniverseVisualizer(val universe: Universe, val zoom: Int) {

  val width: Int = this.universe.getWidth
  val height: Int = this.universe.getHeight

  /**
   * The image.
   */
  private val image = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)
  /**
   * The raster.
   */
  private val raster: WritableRaster = image.getRaster


  /**
   * Create the image.
   *
   * @param pixels The pixels.
   * @param width  The width.
   * @param height The height.
   * @return The image.
   */
  private def createImage(pixels: Array[Int], width: Int, height: Int): Image = {
    this.raster.setPixels(0, 0, this.image.getWidth, this.image.getHeight, pixels)
    image
  }

  /**
   * @return The universe rendered to an image.
   */
  def visualize: Image = {
    val width: Int = this.universe.getWidth
    val height: Int = this.universe.getHeight
    val imageSize: Int = width * height
    val pixels: Array[Int] = new Array[Int](imageSize * this.zoom * this.zoom * 3)
    val rowSize: Int = width * 3 * this.zoom


    for(row <- 0 until height ;
        col <- 0 until width ;
        i <- 0 until 3) {
      val d: Double = (this.universe.get(row, col, i) + 1.0) / 2.0
      for(y <- 0 until zoom;
          x <- 0 until zoom) {
        val idx: Int = (row * this.zoom + y) * rowSize + (col * this.zoom + x) * 3
        pixels(idx + i) = (d * 255.0).asInstanceOf[Int]
      }
    }
    createImage(pixels, width * this.zoom, height * this.zoom)
  }
}