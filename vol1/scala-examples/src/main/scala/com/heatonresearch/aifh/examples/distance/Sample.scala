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

/**
 * Sample: GUI element that displays sampled data.
 * @param width  The width of the downsampled image
 * @param height The height of the downsampled image
 */
@SerialVersionUID(2250441617163548592L)
class Sample(width: Int, height: Int) extends JPanel {
  /**
   * The image data.
   */
  var data = new SampleData(' ', width, height)

  /**
   * @param g Display the downsampled image.
   */
  override def paint(g: Graphics) {
    if (data == null) {
      return
    }
    val vcell: Int = getHeight / data.height
    val hcell: Int = getWidth / data.width
    g.setColor(Color.white)
    g.fillRect(0, 0, getWidth, getHeight)
    g.setColor(Color.black)

    for(y <- 0 until data.height) {
      g.drawLine(0, y * vcell, getWidth, y * vcell)
    }

    for(x <- 0 until data.width) {
      g.drawLine(x * hcell, 0, x * hcell, getHeight)
    }

    for(y <- 0 until data.height;
        x <- 0 until data.width) {
      if (data.getData(x, y))
        g.fillRect(x * hcell, y * vcell, hcell, vcell)
    }

    g.setColor(Color.black)
    g.drawRect(0, 0, getWidth - 1, getHeight - 1)
  }
}
