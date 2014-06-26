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
package com.heatonresearch.aifh.examples.gp

import com.heatonresearch.aifh.AIFHError
import com.heatonresearch.aifh.genetic.trees.EvaluateTree
import com.heatonresearch.aifh.genetic.trees.TreeGenomeNode
import com.heatonresearch.aifh.randomize.GenerateRandom

/**
 * Evaluate expression.  This class shows how to construct a tree evaluator that will evaluate the following
 * operators:
 * <p/>
 * add, sub, div, mul, negative, power, sqrt, as well as variables.
 * <p/>
 * This class could easily be modified to support additional operators.
 */
object EvaluateExpression {
  /**
   * The opcode for add.
   */
  val OPCODE_ADD: Int = 0
  /**
   * The opcode for subtract.
   */
  val OPCODE_SUB: Int = 1
  /**
   * The opcode for divide.
   */
  val OPCODE_DIV: Int = 2
  /**
   * The opcode for multiply.
   */
  val OPCODE_MUL: Int = 3
  /**
   * The opcode for negative.
   */
  val OPCODE_NEG: Int = 4
  /**
   * The opcode for raise to the power.
   */
  val OPCODE_POWER: Int = 5
  /**
   * The opcode for square root.
   */
  val OPCODE_SQRT: Int = 6
  /**
   * The start of the constant and variable opcodes.
   */
  val OPCODE_VAR_CONST: Int = 7
}

/**
 * @param rnd           A random number generator.
 * @param numConst      The number of constants.
 * @param numVar        The number of variables.
 * @param minConstValue The minimum amount for a constant.
 * @param maxConstValue The maximum amount for a constant.
 */
class EvaluateExpression(rnd: GenerateRandom, numConst: Int, numVar: Int, minConstValue: Double,
                         maxConstValue: Double) extends EvaluateTree {

  /**
   * The constant values.
   */
  private val constValues: Array[Double] = {
    val arr = new Array[Double](numConst)
    for(i <- 0 until arr.length)
      arr(i) = rnd.nextDouble(minConstValue, maxConstValue)
    arr
  }

  /**
   * The number of variables.
   */
  private val varCount: Int = numVar

  import EvaluateExpression._

  /**
   * Construct an evaluator with 1 variable, and 100 constants ranging between (-5,5)
   *
   * @param rnd A random number generator.
   */
  def this(rnd: GenerateRandom) {
    this(rnd, 100, 1, -5, 5)
  }

  override def determineChildCount(opcode: Int): Int = {
    opcode match {
      case OPCODE_ADD =>
        2
      case OPCODE_SUB =>
        2
      case OPCODE_DIV =>
        2
      case OPCODE_MUL =>
        2
      case OPCODE_NEG =>
        1
      case OPCODE_POWER =>
        2
      case OPCODE_SQRT =>
        1
      case _ =>
        0
    }
  }

  /**
   * Get the text for an opcode.
   *
   * @param opcode The opcode.
   * @return The text for the opcode.
   */
  def getOpcodeText(opcode: Int): String = {
    opcode match {
      case OPCODE_NEG =>
        "-"
      case OPCODE_ADD =>
        "+"
      case OPCODE_SUB =>
        "-"
      case OPCODE_DIV =>
        "/"
      case OPCODE_MUL =>
        "*"
      case OPCODE_POWER =>
        "^"
      case OPCODE_SQRT =>
        "sqrt"
      case _ =>
        val index: Int = opcode - OPCODE_VAR_CONST
        if (index >= (this.constValues.length + varCount)) {
          throw new AIFHError("Invalid opcode: " + opcode)
        }
        if (index < this.varCount)
          "" + ('a' + index).asInstanceOf[Char]
        else
          "%.8f".format(this.constValues(index - varCount))
    }
  }

  /**
   * Display an expression as LISP (the programming language)
   *
   * @param node The root node.
   * @return The LISP for the expression.
   */
  def displayExpressionLISP(node: TreeGenomeNode): String = {
    val result: StringBuilder = new StringBuilder
    if (determineChildCount(node.opcode) == 0) {
      result.append(getOpcodeText(node.opcode))
    }
    else {
      result.append("(")
      result.append(getOpcodeText(node.opcode))
      import scala.collection.JavaConversions._
      for (child <- node.getChildren) {
        result.append(" ")
        result.append(displayExpressionLISP(child))
      }
      result.append(")")
    }
    result.toString()
  }

  /**
   * Display an expression as normal infix.
   *
   * @param node The root node.
   * @return The infix string.
   */
  def displayExpressionNormal(node: TreeGenomeNode): String = {
    val result: StringBuilder = new StringBuilder
    if (determineChildCount(node.opcode) == 0) {
      result.append(getOpcodeText(node.opcode))
    }
    else {
      val childCount: Int = determineChildCount(node.opcode)
      if (childCount == 0) {
        result.append(getOpcodeText(node.opcode))
      }
      else {
        val name: String = getOpcodeText(node.opcode)
        if (name.length > 1) {
          result.append(name)
          result.append("(")
          var first: Boolean = true
          import scala.collection.JavaConversions._
          for (child <- node.getChildren) {
            if (!first) {
              result.append(",")
            }
            result.append(displayExpressionNormal(child))
            first = false
          }
          result.append(")")
        }
        else {
          result.append("(")
          if (childCount == 2) {
            result.append(displayExpressionNormal(node.getChildren.get(0)))
            result.append(name)
            result.append(displayExpressionNormal(node.getChildren.get(1)))
            result.append(")")
          }
          else {
            result.append(name)
            result.append(displayExpressionNormal(node.getChildren.get(0)))
            result.append(")")
          }
        }
      }
    }
    result.toString()
  }

  override def getVarConstOpcode: Int = OPCODE_VAR_CONST

  override def getNumConst: Int = this.constValues.length

  override def getNumVar: Int = this.varCount

  def evaluate(node: TreeGenomeNode, varValues: Array[Double]): Double = {
    node.opcode match {
      case OPCODE_NEG =>
        -evaluate(node.getChildren.get(0), varValues)
      case OPCODE_ADD =>
        evaluate(node.getChildren.get(0), varValues) + evaluate(node.getChildren.get(1), varValues)
      case OPCODE_SUB =>
        evaluate(node.getChildren.get(0), varValues) - evaluate(node.getChildren.get(1), varValues)
      case OPCODE_DIV =>
        evaluate(node.getChildren.get(0), varValues) / evaluate(node.getChildren.get(1), varValues)
      case OPCODE_MUL =>
        evaluate(node.getChildren.get(0), varValues) * evaluate(node.getChildren.get(1), varValues)
      case OPCODE_POWER =>
        Math.pow(evaluate(node.getChildren.get(0), varValues), evaluate(node.getChildren.get(1), varValues))
      case OPCODE_SQRT =>
        Math.sqrt(evaluate(node.getChildren.get(0), varValues))
      case _ =>
        val index: Int = node.opcode - OPCODE_VAR_CONST
        if (index >= (this.constValues.length + varCount)) {
          throw new AIFHError("Invalid opcode: " + node.opcode)
        }
        if (index < this.varCount)
          varValues(index)
        else
          this.constValues(index - this.varCount)
    }
  }
}