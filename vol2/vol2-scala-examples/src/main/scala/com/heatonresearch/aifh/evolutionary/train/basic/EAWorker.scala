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
package com.heatonresearch.aifh.evolutionary.train.basic

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.species.Species
import com.heatonresearch.aifh.randomize.GenerateRandom
import java.util.concurrent.Callable
import scala.util.control.ControlThrowable

/**
 * A worker thread for an Evolutionary Algorithm.
 *
 * @param train   The trainer.
 * @param species The species being processed.
 */
class EAWorker(train: BasicEA, species: Species) extends Callable[AnyRef] {

  /**
   * The parent genomes.
   */
  private val parents: Array[Genome] = new Array[Genome](train.getOperators.maxParents)
  /**
   * The children genomes.
   */
  private val children: Array[Genome] = new Array[Genome](train.getOperators.maxOffspring)
  /**
   * Random number generator.
   */
  private val rnd: GenerateRandom = train.randomNumberFactory.factor

  /**
   * Choose a parent.
   *
   * @return The chosen parent.
   */
  private def chooseParent: Genome = {
    val idx: Int = train.selection.performSelection(rnd, species)
    species.members(idx)
  }

  override def call: AnyRef = {
    var success = false
    var tries = train.maxOperationErrors
    while(!success) {
      try {
        val opp = train.getOperators.pickMaxParents(rnd, species.members.size)
        children(0) = null
        parents(0) = chooseParent
        if (opp.parentsNeeded > 1) {
          var numAttempts: Int = 5
          parents(1) = chooseParent
          while ((parents(0) eq parents(1)) && (numAttempts > 0)) {
            parents(1) = chooseParent
          }
          numAttempts += 1
          if (parents(0) ne parents(1)) {
            opp.performOperation(rnd, parents, 0, children, 0)
          }
        }
        else {
          opp.performOperation(rnd, parents, 0, children, 0)
          children(0).population = parents(0).population
        }
        for (child <- children) {
          if (child != null) {
            child.population = parents(0).population
            child.birthGeneration = train.getIteration
            train.calculateScore(child)
            if (!train.addChild(child)) {
              return null
            }
            success = true
          }
        }
      } catch {
        case e: AIFHError =>
          tries -= 1
          if (tries < 0) {
            throw new AIFHError(s"Could not perform a successful genetic operation after ${train.maxOperationErrors} tries.")
          }
        case t: ControlThrowable => throw t
        case t: Throwable =>
          if (!train.shouldIgnoreExceptions) {
            train.reportError(t)
          }
      }
    }
    null
  }
}