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
package com.heatonresearch.aifh.examples.gp;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.genetic.trees.EvaluateTree;
import com.heatonresearch.aifh.genetic.trees.TreeGenomeNode;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.Locale;

/**
 * Evaluate expression.  This class shows how to construct a tree evaluator that will evaluate the following
 * operators:
 * <p/>
 * add, sub, div, mul, negative, power, sqrt, as well as variables.
 * <p/>
 * This class could easily be modified to support additional operators.
 */
public class EvaluateExpression extends EvaluateTree {
    /**
     * The opcode for add.
     */
    public static final int OPCODE_ADD = 0;

    /**
     * The opcode for subtract.
     */
    public static final int OPCODE_SUB = 1;

    /**
     * The opcode for divide.
     */
    public static final int OPCODE_DIV = 2;

    /**
     * The opcode for multiply.
     */
    public static final int OPCODE_MUL = 3;

    /**
     * The opcode for negative.
     */
    public static final int OPCODE_NEG = 4;

    /**
     * The opcode for raise to the power.
     */
    public static final int OPCODE_POWER = 5;

    /**
     * The opcode for square root.
     */
    public static final int OPCODE_SQRT = 6;

    /**
     * The start of the constant and variable opcodes.
     */
    public static final int OPCODE_VAR_CONST = 7;

    /**
     * The constant values.
     */
    private double[] constValues;

    /**
     * The number of variables.
     */
    private int varCount;

    /**
     * The constructor.
     *
     * @param rnd           A random number generator.
     * @param numConst      The number of constants.
     * @param numVar        The number of variables.
     * @param minConstValue The minimum amount for a constant.
     * @param maxConstValue The maximum amount for a constant.
     */
    public EvaluateExpression(GenerateRandom rnd, int numConst, int numVar, double minConstValue, double maxConstValue) {
        this.constValues = new double[numConst];
        this.varCount = numVar;

        for (int i = 0; i < this.constValues.length; i++) {
            this.constValues[i] = rnd.nextDouble(minConstValue, maxConstValue);
        }
    }

    /**
     * Construct an evaluator with 1 variable, and 100 constants ranging between (-5,5)
     *
     * @param rnd A random number generator.
     */
    public EvaluateExpression(GenerateRandom rnd) {
        this(rnd, 100, 1, -5, 5);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int determineChildCount(int opcode) {
        switch (opcode) {
            case OPCODE_ADD:
                return 2;
            case OPCODE_SUB:
                return 2;
            case OPCODE_DIV:
                return 2;
            case OPCODE_MUL:
                return 2;
            case OPCODE_NEG:
                return 1;
            case OPCODE_POWER:
                return 2;
            case OPCODE_SQRT:
                return 1;
            default:
                return 0;
        }
    }

    /**
     * Get the text for an opcode.
     *
     * @param opcode The opcode.
     * @return The text for the opcode.
     */
    public String getOpcodeText(int opcode) {
        switch (opcode) {
            case OPCODE_NEG:
                return "-";
            case OPCODE_ADD:
                return "+";
            case OPCODE_SUB:
                return "-";
            case OPCODE_DIV:
                return "/";
            case OPCODE_MUL:
                return "*";
            case OPCODE_POWER:
                return "^";
            case OPCODE_SQRT:
                return "sqrt";
            default:
                int index = opcode - OPCODE_VAR_CONST;
                if (index >= (this.constValues.length + varCount)) {
                    throw new AIFHError("Invalid opcode: " + opcode);
                }
                if (index < this.varCount) {
                    return "" + ((char) ('a' + index));
                } else {
                    return String.format(Locale.US, "%.8f", this.constValues[index - varCount]);
                }
        }
    }


    /**
     * Display an expression as LISP (the programming language)
     *
     * @param node The root node.
     * @return The LISP for the expression.
     */
    public String displayExpressionLISP(TreeGenomeNode node) {
        StringBuilder result = new StringBuilder();

        if (determineChildCount(node.getOpcode()) == 0) {
            result.append(getOpcodeText(node.getOpcode()));
        } else {
            result.append("(");
            result.append(getOpcodeText(node.getOpcode()));
            for (TreeGenomeNode child : node.getChildren()) {
                result.append(" ");
                result.append(displayExpressionLISP(child));
            }
            result.append(")");
        }

        return result.toString();
    }


    /**
     * Display an expression as normal infix.
     *
     * @param node The root node.
     * @return The infix string.
     */
    public String displayExpressionNormal(TreeGenomeNode node) {
        StringBuilder result = new StringBuilder();

        if (determineChildCount(node.getOpcode()) == 0) {
            result.append(getOpcodeText(node.getOpcode()));
        } else {
            int childCount = determineChildCount(node.getOpcode());

            if (childCount == 0) {
                result.append(getOpcodeText(node.getOpcode()));
            } else {
                String name = getOpcodeText(node.getOpcode());

                if (name.length() > 1) {
                    result.append(name);
                    result.append("(");
                    boolean first = true;
                    for (TreeGenomeNode child : node.getChildren()) {
                        if (!first) {
                            result.append(",");
                        }
                        result.append(displayExpressionNormal(child));
                        first = false;
                    }
                    result.append(")");
                } else {
                    result.append("(");
                    if (childCount == 2) {
                        result.append(displayExpressionNormal(node.getChildren().get(0)));
                        result.append(name);
                        result.append(displayExpressionNormal(node.getChildren().get(1)));
                        result.append(")");
                    } else {
                        result.append(name);
                        result.append(displayExpressionNormal(node.getChildren().get(0)));
                        result.append(")");
                    }
                }
            }

        }


        return result.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getVarConstOpcode() {
        return OPCODE_VAR_CONST;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNumConst() {
        return this.constValues.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNumVar() {
        return this.varCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double evaluate(TreeGenomeNode node, double[] varValues) {
        switch (node.getOpcode()) {
            case OPCODE_NEG:
                return -(evaluate(node.getChildren().get(0), varValues));
            case OPCODE_ADD:
                return evaluate(node.getChildren().get(0), varValues) + evaluate(node.getChildren().get(1), varValues);
            case OPCODE_SUB:
                return evaluate(node.getChildren().get(0), varValues) - evaluate(node.getChildren().get(1), varValues);
            case OPCODE_DIV:
                return evaluate(node.getChildren().get(0), varValues) / evaluate(node.getChildren().get(1), varValues);
            case OPCODE_MUL:
                return evaluate(node.getChildren().get(0), varValues) * evaluate(node.getChildren().get(1), varValues);
            case OPCODE_POWER:
                return Math.pow(evaluate(node.getChildren().get(0), varValues), evaluate(node.getChildren().get(1), varValues));
            case OPCODE_SQRT:
                return Math.sqrt(evaluate(node.getChildren().get(0), varValues));
            default:
                int index = node.getOpcode() - OPCODE_VAR_CONST;
                if (index >= (this.constValues.length + varCount)) {
                    throw new AIFHError("Invalid opcode: " + node.getOpcode());
                }
                if (index < this.varCount) {
                    return varValues[index];
                } else {
                    return this.constValues[index - this.varCount];
                }
        }

    }
}
