// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
using System;
using System.Globalization;
using System.Text;
using AIFH_Vol2.Core;
using AIFH_Vol2.Core.Genetic.Trees;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Examples.GP
{
    /// <summary>
    /// Evaluate expression.  This class shows how to construct a tree evaluator that will evaluate the following
    /// operators:
    ///
    /// add, sub, div, mul, negative, power, sqrt, as well as variables.
    ///
    /// This class could easily be modified to support additional operators.
    /// </summary>
    public class EvaluateExpression : EvaluateTree
    {
        /// <summary>
        ///     The opcode for add.
        /// </summary>
        public const int OPCODE_ADD = 0;

        /// <summary>
        ///     The opcode for subtract.
        /// </summary>
        public const int OPCODE_SUB = 1;

        /// <summary>
        ///     The opcode for divide.
        /// </summary>
        public const int OPCODE_DIV = 2;

        /// <summary>
        ///     The opcode for multiply.
        /// </summary>
        public const int OPCODE_MUL = 3;

        /// <summary>
        ///     The opcode for negative.
        /// </summary>
        public const int OPCODE_NEG = 4;

        /// <summary>
        ///     The opcode for raise to the power.
        /// </summary>
        public const int OPCODE_POWER = 5;

        /// <summary>
        ///     The opcode for square root.
        /// </summary>
        public const int OPCODE_SQRT = 6;

        /// <summary>
        ///     The start of the constant and variable opcodes.
        /// </summary>
        public const int OPCODE_VAR_CONST = 7;

        /// <summary>
        ///     The constant values.
        /// </summary>
        private readonly double[] constValues;

        /// <summary>
        ///     The number of variables.
        /// </summary>
        private readonly int varCount;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="numConst">The number of constants.</param>
        /// <param name="numVar">The number of variables.</param>
        /// <param name="minConstValue">The minimum amount for a constant.</param>
        /// <param name="maxConstValue">The maximum amount for a constant.</param>
        public EvaluateExpression(IGenerateRandom rnd, int numConst, int numVar, double minConstValue,
            double maxConstValue)
        {
            constValues = new double[numConst];
            varCount = numVar;

            for (int i = 0; i < constValues.Length; i++)
            {
                constValues[i] = rnd.NextDouble(minConstValue, maxConstValue);
            }
        }

        /**
         * Construct an evaluator with 1 variable, and 100 constants ranging between (-5,5)
         *
         * @param rnd A random number generator.
         */

        public EvaluateExpression(IGenerateRandom rnd)
            : this(rnd, 100, 1, -5, 5)
        {
        }

        /// <inheritdoc />
        public override int VarConstOpcode
        {
            get { return OPCODE_VAR_CONST; }
        }

        /// <inheritdoc />
        public override int NumConst
        {
            get { return constValues.Length; }
        }

        /// <inheritdoc />
        public override int NumVar
        {
            get { return varCount; }
        }

        /// <inheritdoc />
        public override int DetermineChildCount(int opcode)
        {
            switch (opcode)
            {
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

        /// <summary>
        ///     Get the text for an opcode.
        /// </summary>
        /// <param name="opcode">The opcode.</param>
        /// <returns>The text for the opcode.</returns>
        public String GetOpcodeText(int opcode)
        {
            switch (opcode)
            {
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
                    if (index >= (constValues.Length + varCount))
                    {
                        throw new AIFHError("Invalid opcode: " + opcode);
                    }
                    if (index < varCount)
                    {
                        return "" + ((char)('a' + index));
                    }
                    return constValues[index - varCount].ToString(CultureInfo.InvariantCulture);
            }
        }

        /// <summary>
        ///     Display an expression as LISP (the programming language)
        /// </summary>
        /// <param name="node">The root node.</param>
        /// <returns>The LISP for the expression.</returns>
        public String DisplayExpressionLISP(TreeGenomeNode node)
        {
            var result = new StringBuilder();

            if (DetermineChildCount(node.Opcode) == 0)
            {
                result.Append(GetOpcodeText(node.Opcode));
            }
            else
            {
                result.Append("(");
                result.Append(GetOpcodeText(node.Opcode));
                foreach (TreeGenomeNode child in node.Children)
                {
                    result.Append(" ");
                    result.Append(DisplayExpressionLISP(child));
                }
                result.Append(")");
            }

            return result.ToString();
        }

        /// <summary>
        ///     Display an expression as normal infix.
        /// </summary>
        /// <param name="node">The root node.</param>
        /// <returns>The infix string.</returns>
        public String DisplayExpressionNormal(TreeGenomeNode node)
        {
            var result = new StringBuilder();

            if (DetermineChildCount(node.Opcode) == 0)
            {
                result.Append(GetOpcodeText(node.Opcode));
            }
            else
            {
                int childCount = DetermineChildCount(node.Opcode);

                if (childCount == 0)
                {
                    result.Append(GetOpcodeText(node.Opcode));
                }
                else
                {
                    String name = GetOpcodeText(node.Opcode);

                    if (name.Length > 1)
                    {
                        result.Append(name);
                        result.Append("(");
                        bool first = true;
                        foreach (TreeGenomeNode child in node.Children)
                        {
                            if (!first)
                            {
                                result.Append(",");
                            }
                            result.Append(DisplayExpressionNormal(child));
                            first = false;
                        }
                        result.Append(")");
                    }
                    else
                    {
                        result.Append("(");
                        if (childCount == 2)
                        {
                            result.Append(DisplayExpressionNormal(node.Children[0]));
                            result.Append(name);
                            result.Append(DisplayExpressionNormal(node.Children[1]));
                            result.Append(")");
                        }
                        else
                        {
                            result.Append(name);
                            result.Append(DisplayExpressionNormal(node.Children[0]));
                            result.Append(")");
                        }
                    }
                }
            }


            return result.ToString();
        }


        /// <inheritdoc />
        public override double Evaluate(TreeGenomeNode node, double[] varValues)
        {
            switch (node.Opcode)
            {
                case OPCODE_NEG:
                    return -(Evaluate(node.Children[0], varValues));
                case OPCODE_ADD:
                    return Evaluate(node.Children[0], varValues) + Evaluate(node.Children[1], varValues);
                case OPCODE_SUB:
                    return Evaluate(node.Children[0], varValues) - Evaluate(node.Children[1], varValues);
                case OPCODE_DIV:
                    return Evaluate(node.Children[0], varValues) / Evaluate(node.Children[1], varValues);
                case OPCODE_MUL:
                    return Evaluate(node.Children[0], varValues) * Evaluate(node.Children[1], varValues);
                case OPCODE_POWER:
                    return Math.Pow(Evaluate(node.Children[0], varValues), Evaluate(node.Children[1], varValues));
                case OPCODE_SQRT:
                    return Math.Sqrt(Evaluate(node.Children[0], varValues));
                default:
                    int index = node.Opcode - OPCODE_VAR_CONST;
                    if (index >= (constValues.Length + varCount))
                    {
                        throw new AIFHError("Invalid opcode: " + node.Opcode);
                    }
                    if (index < varCount)
                    {
                        return varValues[index];
                    }
                    return constValues[index - varCount];
            }
        }
    }
}
