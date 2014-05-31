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
package com.heatonresearch.aifh.error

/**
 * The sum of squares method (SSE) measures the error as the sum of the squared difference of each vector element.
 * <p/>
 * http://www.heatonresearch.com/wiki/Sum_of_Squares_Error
 */
class ErrorCalculationSSE extends AbstractErrorCalculation {
  /**
   * Calculate the error with SSE.
   *
   * @return The current error.
   */
  def calculate: Double = {
    if (setSize == 0)
      Double.PositiveInfinity
    else
      globalError
  }

  def create: ErrorCalculation = new ErrorCalculationSSE
}