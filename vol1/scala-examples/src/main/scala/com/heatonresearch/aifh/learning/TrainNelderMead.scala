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
package com.heatonresearch.aifh.learning

import com.heatonresearch.aifh.learning.score.ScoreFunction
import scala.collection.mutable.ArrayBuffer
import com.heatonresearch.aifh.general.data.RichData
import RichData._

/**
 * The Nelder-Mead method is a commonly used parameter optimization method that
 * can be used for machine learning. It typically provides a good error
 * rate and is relatively fast.
 * <p/>
 * Nelder-Mead must build a simplex, which is an n*(n+1) matrix of weights. If
 * you have a large number of weights, this matrix can quickly overflow memory.
 * <p/>
 * The biggest enhancement that is needed for this trainer is to make use of
 * multi-threaded code to evaluate the speed evaluations when training on a
 * multi-core.
 * <p/>
 * This implementation is based on the source code provided by John Burkardt
 * (http://people.sc.fsu.edu/~jburkardt/)
 * <p/>
 * http://people.sc.fsu.edu/~jburkardt/c_src/asa047/asa047.c
 */
object TrainNelderMead {
  /**
   * Used to calculate the centroid.
   */
  val CCOEFF = 0.5
  val ECOEFF = 2.0
  val EPS = 0.001
  val RCOEFF = 1.0
}

class TrainNelderMead(algorithm: MachineLearningAlgorithm, score: ScoreFunction, stepValue: Double = 100) extends LearningMethod {
  import TrainNelderMead._

  private val start: ArrayBuffer[Double] = algorithm.longTermMemory.clone()
  private val trainedWeights: ArrayBuffer[Double] = algorithm.longTermMemory.clone()
  val n: Int = start.length
  private val p = ArrayBuffer.fill(n * (n + 1))(0.0)
  private val pstar = ArrayBuffer.fill(n)(0.0)
  private val p2star = ArrayBuffer.fill(n)(0.0)
  private val pbar = ArrayBuffer.fill(n)(0.0)
  private val y = ArrayBuffer.fill(n + 1)(0.0)
  private val nn = n + 1
  private var del: Double = 1.0
  private val rq = 0.000001 * n
  private val step = ArrayBuffer.fill(start.length)(stepValue)
  private val konvge = 500
  private var jcount = konvge

  /**
   * Calculate the error for the neural network with a given set of weights.
   *
   * @param weights The weights to use.
   * @return The current error.
   */
  def fn(weights: ArrayBuffer[Double]): Double = {
    algorithm.longTermMemory.set(weights)
    score.calculateScore(algorithm)
  }

  override def done: Boolean = converged

  override def getStatus: String = ""

  override def iteration() {
    if (converged)
      return

    val n: Int = start.length
    p.setFrom(start,0,n*n,n)
    y(n) = fn(start)
    for(j <- 0 until n) {
      val x = start(j)
      start(j) = start(j) + step(j) * del
      p.setFrom(start,0,j * n,n)
      y(j) = fn(start)
      start(j) = x
    }

    /*
     * The simplex construction is complete.
     *
     * Find highest and lowest Y values. YNEWLO = Y(IHI) indicates the
     * vertex of the simplex to be replaced.
     */
    var ylo: Double = y(0)
    var ilo: Int = 0
    for(i <- 1 until nn) {
      if (y(i) < ylo) {
        ylo = y(i)
        ilo = i
      }
    }

    /*
     * Inner loop.
     */
    var ynewlo: Double = .0

    class ContinueException extends Exception

    var z: Double = .0
    try {
      while (true) {
        /*
         * if (kcount <= icount) { break; }
         */
        try {
          ynewlo = y(0)
          var ihi: Int = 0
          for(i <- 1 until nn) {
            if (ynewlo < y(i)) {
              ynewlo = y(i)
              ihi = i
            }
          }
          /*
           * Calculate PBAR, the centroid of the simplex vertices excepting
           * the vertex with Y value YNEWLO.
           */
          for(i <- 0 until n) {
            z = 0.0
            for(j <- 0 until nn) {
              z = z + p(i + j * n)
            }
            z = z - p(i + ihi * n)
            pbar(i) = z / n
          }
          /*
           * Reflection through the centroid.
           */
          pstar.updateValues((i,_) => pbar(i) + RCOEFF * (pbar(i) - p(i + ihi * n)))

          val ystar: Double = fn(pstar)

          /*
           * Successful reflection, so extension.
           */
          var y2star: Double = .0
          if (ystar < ylo) {
            p2star.updateValues((i,_) => pbar(i) + ECOEFF * (pstar(i) - pbar(i)))
            y2star = fn(p2star)

            // Check extension.
            if (ystar < y2star) {
              p.setFrom(pstar,0,ihi * n,n)
              y(ihi) = ystar
            }
            else {
              // Retain extension or contraction.
              p.setFrom(p2star,0,ihi * n,n)
              y(ihi) = y2star
            }
          }
          else {
            // No extension.

            var l: Int = 0
            for(i <- 0 until nn) {
              if (ystar < y(i)) {
                l = l + 1
              }
            }
            if (l > 1) {
              p.setFrom(pstar,0,ihi * n,n)
              y(ihi) = ystar
            }
            else if (l == 0) {
              // Contraction on the Y(IHI) side of the centroid.
                p2star.updateValues((i,_) => pbar(i) + CCOEFF * (p(i + ihi * n) - pbar(i)))
              y2star = fn(p2star)
              // Contract the whole simplex.
              if (y(ihi) < y2star) {
                for(j <- 0 until nn) {
                  for(i <- 0 until n) {
                    p(i + j * n) = (p(i + j * n) + p(i + ilo * n)) * 0.5
                    trainedWeights(i) = p(i + j * n)
                  }
                  y(j) = fn(trainedWeights)
                }
                ylo = y(0)
                ilo = 0
                for(i <- 0 until nn) {
                  if (y(i) < ylo) {
                    ylo = y(i)
                    ilo = i
                  }
                }
                throw new ContinueException
              }
              else {
                // Retain contraction.
                p.setFrom(p2star,0,ihi * n,n)
                y(ihi) = y2star
              }
            }
            else if (l == 1) {
              // Contraction on the reflection side of the centroid.
              p2star.updateValues((i,v) => pbar(i) + CCOEFF * (pstar(i) - pbar(i)))
              y2star = fn(p2star)

              // Retain reflection?
              if (y2star <= ystar) {
                p.setFrom(p2star,0,ihi * n,n)
                y(ihi) = y2star
              }
              else {
                p.setFrom(pstar,0,ihi * n,n)
                y(ihi) = ystar
              }
            }
          }

          // Check if YLO improved.
          if (y(ihi) < ylo) {
            ylo = y(ihi)
            ilo = ihi
          }
          jcount -= 1
          if (jcount > 0) {
            throw new ContinueException
          }

          // Check to see if minimum reached.
          jcount = konvge
          z = 0.0
          for(i <- 0 until nn) {
            z = z + y(i)
          }
          val x: Double = z / nn
          z = 0.0
          for(i <- 0 until nn) {
            z = z + Math.pow(y(i) - x, 2)
          }
          if (z <= rq) {
            throw new BreakException
          }
        }
        catch {
          case ce: ContinueException => {}
        }
      }
    }
    catch {
      case be: TrainNelderMead#BreakException => {}
    }

    // Factorial tests to check that YNEWLO is a local minimum.
    trainedWeights.setFrom(p,ilo * n,0,n)
    ynewlo = y(ilo)
    var fault = false
    try {
      for(i <- 0 until n) {
        del = step(i) * EPS
        trainedWeights(i) += del
        z = fn(trainedWeights)
        if (z < ynewlo) {
          fault = true
          throw new BreakException
        }
        trainedWeights(i) = trainedWeights(i) - del - del
        z = fn(trainedWeights)
        if (z < ynewlo) {
          fault = true
          throw new BreakException
        }
        trainedWeights(i) += del
      }
    } catch {
      case be: BreakException =>
    }
    if (!fault) {
      converged = true
    } else {
      // Restart the procedure.
      start.set(trainedWeights)
      del = EPS
    }
    lastError = ynewlo
    algorithm.longTermMemory.set(trainedWeights)
  }

  override def getLastError: Double = lastError

  override def finishTraining() {}

  /**
   * True if the network has converged, and no further training is needed.
   */
  private var converged: Boolean = false

  private var lastError: Double = .0

  private[learning] class BreakException extends Exception {}

}
