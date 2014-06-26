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
import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC
import com.heatonresearch.aifh.evolutionary.codec.GenomeAsPhenomeCODEC
import com.heatonresearch.aifh.evolutionary.genome.Genome
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator
import com.heatonresearch.aifh.evolutionary.opp.OperationList
import com.heatonresearch.aifh.evolutionary.opp.selection.SelectionOperator
import com.heatonresearch.aifh.evolutionary.opp.selection.TournamentSelection
import com.heatonresearch.aifh.evolutionary.population.Population
import com.heatonresearch.aifh.evolutionary.score.AdjustScore
import com.heatonresearch.aifh.evolutionary.score.parallel.ParallelScore
import com.heatonresearch.aifh.evolutionary.sort._
import com.heatonresearch.aifh.evolutionary.species.SingleSpeciation
import com.heatonresearch.aifh.evolutionary.species.Speciation
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm
import com.heatonresearch.aifh.learning.LearningMethod
import com.heatonresearch.aifh.learning.MLContext
import com.heatonresearch.aifh.learning.MLMethod
import com.heatonresearch.aifh.learning.score.ScoreFunction
import com.heatonresearch.aifh.randomize.MersenneTwisterFactory
import com.heatonresearch.aifh.randomize.RandomFactory
import java.io.Serializable
import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
* Provides a basic implementation of a multi-threaded Evolutionary Algorithm.
* The EA works from a score function.
*/
object BasicEA {
  /**
   * Calculate the score adjustment, based on adjusters.
   *
   * @param genome    The genome to adjust.
   * @param adjusters The score adjusters.
   */
  def calculateScoreAdjustment(genome: Genome, adjusters: List[AdjustScore]) {
    val score = genome.score
    var delta = 0.0
    for (a <- adjusters) {
      delta += a.calculateAdjustment(genome)
    }
    genome.adjustedScore = score + delta
  }
}

/**
* Construct an EA.
*
* @param thePopulation    The population.
* @param theScoreFunction The score function.
*/
@SerialVersionUID(1L)
class BasicEA(thePopulation: Population, theScoreFunction: ScoreFunction) extends EvolutionaryAlgorithm with LearningMethod with Serializable {
  /**
   * Should exceptions be ignored.
   */
  override var shouldIgnoreExceptions: Boolean = false
  /**
   * The genome comparator.
   */
  override var bestComparator: GenomeComparator = null
  /**
   * The genome comparator.
   */
  override var selectionComparator: GenomeComparator = null
  
  
  /**
   * The score adjusters.
   */
  var scoreAdjusters: List[AdjustScore] = Nil
  /**
   * The operators. to use.
   */
  private val operators: OperationList = new OperationList
  /**
   * The CODEC to use to convert between genome and phenome.
   */
  var codec: GeneticCODEC = new GenomeAsPhenomeCODEC
  /**
   * Random number factory.
   */
  var randomNumberFactory: RandomFactory = new MersenneTwisterFactory
  /**
   * The validation mode.
   */
  var validationMode: Boolean = false
  /**
   * The iteration number.
   */
  private var iterationCount: Int = 0
  /**
   * The desired thread count.
   */
  var threadCount: Int = 0
  /**
   * The actual thread count.
   */
  private var actualThreadCount: Int = -1
  /**
   * The speciation method.
   */
  var speciation: Speciation = new SingleSpeciation
  /**
   * This property stores any error that might be reported by a thread.
   */
  private var reportedError: Throwable = null
  /**
   * The best genome from the last iteration.
   */
  private var oldBestGenome: Genome = null
  /**
   * The population for the next iteration.
   */
  private val newPopulation: java.util.List[Genome] = new java.util.ArrayList[Genome]
  /**
   * The mutation to be used on the top genome. We want to only modify its
   * weights.
   */
  var champMutation: EvolutionaryOperator = null
  /**
   * The percentage of a species that is "elite" and is passed on directly.
   */
  var eliteRate: Double = 0.3
  /**
   * The number of times to try certain operations, so an endless loop does
   * not occur.
   */
  var maxTries: Int = 5
  /**
   * The best ever genome.
   */
  private var bestGenome: Genome = null
  /**
   * The thread pool executor.
   */
  private var taskExecutor: ExecutorService = null
  /**
   * Holds the threads used each iteration.
   */
  private val threadList: java.util.List[Callable[AnyRef]] = new java.util.ArrayList[Callable[AnyRef]]

  var maxOperationErrors: Int = 500

  override var population = thePopulation
  override val scoreFunction = theScoreFunction
  /**
   * The selection operator.
   */
  override val selection: SelectionOperator = new TournamentSelection(this, 4)

  if (theScoreFunction.shouldMinimize) {
    selectionComparator = new MinimizeAdjustedScoreComp
    bestComparator = new MinimizeScoreComp
  } else {
    selectionComparator = new MaximizeAdjustedScoreComp
    bestComparator = new MaximizeScoreComp
  }
  for (species <- thePopulation.speciesList ;
       genome <- species.members) {
    setIteration(Math.max(getIteration, genome.birthGeneration))
  }

  if (population.speciesList.size > 0 && population.speciesList(0).members.size > 0) {
    bestGenome = population.speciesList(0).members(0)
  }

  //----end of construction----
  /**
   * Add a child to the next iteration.
   *
   * @param genome The child.
   * @return True, if the child was added successfully.
   */
  def addChild(genome: Genome): Boolean = {
    newPopulation synchronized {
      if (newPopulation.size < population.populationSize) {
        if (genome ne oldBestGenome) {
          if (validationMode) {
            if (newPopulation.contains(genome)) {
              throw new AIFHError("Genome already added to population: " + genome.toString)
            }
          }
          newPopulation.add(genome)
        }
        if (!genome.score.isInfinite && !genome.score.isNaN && bestComparator.isBetterThan(genome, bestGenome)) {
          bestGenome = genome
          population.bestGenome =bestGenome
        }
        return true
      }
      else {
        return false
      }
    }
  }

  override def addOperation(probability: Double, opp: EvolutionaryOperator) {
    getOperators.add(probability, opp)
  }

  override def addScoreAdjuster(scoreAdjust: AdjustScore) {
    scoreAdjusters ::= scoreAdjust
  }

  override def calculateScore(g: Genome) {
    val phenotype: MLMethod = codec.decode(g)
    var score: Double = .0
    if (phenotype == null) {
      if (bestComparator.shouldMinimize) {
        score = Double.PositiveInfinity
      }
      else {
        score = Double.NegativeInfinity
      }
    }
    else {
      phenotype match {
        case context: MLContext =>
          context.clearContext()
        case _ =>
      }
      score = scoreFunction.calculateScore(phenotype)
    }
    g.score = score
    g.adjustedScore =score
  }

  override def finishTraining() {
    if (taskExecutor != null) {
      taskExecutor.shutdown()
      try {
        taskExecutor.awaitTermination(Long.MaxValue, TimeUnit.MINUTES)
      }
      catch {
        case e: InterruptedException =>
          throw new AIFHError(e)
      }
      finally {
        taskExecutor = null
      }
    }
  }

  override def getBestGenome: Genome = bestGenome

  override def getLastError: Double = {
    if (bestGenome != null) {
      val err: Double = bestGenome.score
      if (!err.isNaN) {
        return err
      }
    }
    if (scoreFunction.shouldMinimize)
      Double.PositiveInfinity
    else
      Double.NegativeInfinity
  }

  override def getIteration: Int = iterationCount

  override def getMaxIndividualSize: Int = population.getMaxIndividualSize

  /**
   * @return the oldBestGenome
   */
  def getOldBestGenome: Genome = oldBestGenome

  override def getOperators: OperationList = operators

  override def iteration() {
    if (actualThreadCount == -1) {
      preIteration()
    }
    if (population.speciesList.isEmpty) {
      throw new AIFHError("Population is empty, there are no species.")
    }
    iterationCount += 1
    newPopulation.clear()
    newPopulation.add(bestGenome)
    oldBestGenome = bestGenome
    threadList.clear()
    class BreakException extends Exception

    try {
      for (species <- population.speciesList) {
        var numToSpawn = species.offspringCount
        if (species.members.size > 5) {
          val idealEliteCount = (species.members.size * eliteRate).asInstanceOf[Int]
          val eliteCount = Math.min(numToSpawn, idealEliteCount)
          for (i <- 0 until eliteCount) {
            val eliteGenome = species.members(i)
            if (getOldBestGenome ne eliteGenome) {
              numToSpawn -= 1
              if (!addChild(eliteGenome)) {
                throw new BreakException
              }
            }
          }
        }
        while ( {
          numToSpawn -= 1
          numToSpawn + 1
        } > 0) {
          val worker: EAWorker = new EAWorker(this, species)
          threadList.add(worker)
        }
      }
    } catch {
      case _ : BreakException =>
    }
    try {
      taskExecutor.invokeAll(threadList)
    } catch {
      case e: InterruptedException =>
        e.printStackTrace()
    }
    if (reportedError != null && !shouldIgnoreExceptions) {
      throw new AIFHError(reportedError)
    }
    if (validationMode) {
      if (oldBestGenome != null && !newPopulation.contains(oldBestGenome)) {
        throw new AIFHError("The top genome died, this should never happen!!")
      }
      if (bestGenome != null && oldBestGenome != null && bestComparator.isBetterThan(oldBestGenome, bestGenome)) {
        throw new AIFHError(s"The best genome's score got worse, this should never happen!! Went from ${oldBestGenome.score} to ${bestGenome.score}")
      }
    }
    import scala.collection.JavaConversions._
    speciation.performSpeciation(newPopulation.toList)
    population.purgeInvalidGenomes()
  }

  def done: Boolean = false

  def getStatus: String = "Species Count: " + population.speciesList.size

  def performShutdownTask() {
    finishTraining()
  }

  /**
   * Called before the first iteration. Determine the number of threads to
   * use.
   */
  private def preIteration() {
    speciation.init(this)
    if (threadCount == 0)
      actualThreadCount = Runtime.getRuntime.availableProcessors
    else
      actualThreadCount = threadCount

    val pscore = new ParallelScore(population, codec, List.empty[AdjustScore], scoreFunction, actualThreadCount)
    pscore.requestedThreads = actualThreadCount
    pscore.process()
    actualThreadCount = pscore.requestedThreads
    if (actualThreadCount == 1)
      taskExecutor = Executors.newSingleThreadScheduledExecutor
    else
      taskExecutor = Executors.newFixedThreadPool(actualThreadCount)

    val list = population.flatten
    var idx: Int = 0
    do {
      bestGenome = list(idx)
      idx += 1
    } while (idx < list.size && (bestGenome.score.isInfinite || bestGenome.score.isNaN))
    population.bestGenome = bestGenome
    for (species <- population.speciesList) {
      if (species.leader == null && species.members.size > 0) {
        species.leader = species.members(0)
      }
    }
    val genomes = population.flatten
    speciation.performSpeciation(genomes)
    population.purgeInvalidGenomes()
  }

  /**
   * Called by a thread to report an error.
   *
   * @param t The error reported.
   */
  def reportError(t: Throwable) {
    this synchronized {
      if (reportedError == null) {
        reportedError = t
      }
    }
  }

  /**
   * Set the current iteration number.
   *
   * @param iteration The iteration number.
   */
  def setIteration(iteration: Int) {
    iterationCount = iteration
  }
}