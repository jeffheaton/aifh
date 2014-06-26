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
package com.heatonresearch.aifh.examples.ca.mergelife.viewer

import java.io.File
import java.io.Serializable

/**
 * Configuration data for the dialog.
 */
@SerialVersionUID(1L)
class ConfigData extends Serializable {

  /**
   * @return the zoom
   */
  def getZoom: Int = {
    if (zoom < 1) {
      zoom = 1
    }
    zoom
  }

  /**
   * @param zoom the zoom to set
   */
  def setZoom(zoom: Int) {
    this.zoom = zoom
  }

  /**
   * The universe height.
   */
  var paneHeight: Int = 200
  /**
   * The universe width.
   */
  var paneWidth: Int = 200
  /**
   * The save directory.
   */
  var saveDirectory: File = new File(".")
  /**
   * How many universes across.
   */
  var universePaneColumns: Int = 7
  /**
   * How many universes down.
   */
  var universePaneRows: Int = 4
  /**
   * The zoom factor.
   */
  private var zoom: Int = 1
}