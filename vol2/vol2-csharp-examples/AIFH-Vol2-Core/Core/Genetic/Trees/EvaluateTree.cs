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
using System.Collections.Generic;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     Evaluate a tree.  Used for genetic programming.
    /// </summary>
    public abstract class EvaluateTree
    {
        /// <summary>
        /// The first opcode for the variable and constant nodes.
        /// </summary>
        public abstract int VarConstOpcode { get; }

        /// <summary>
        /// The number of constants supported.
        /// </summary>
        public abstract int NumConst { get; }

        /// <summary>
        /// The number of variables supported.
        /// </summary>
        public abstract int NumVar { get; }

        /// <summary>
        /// The total number of opcodes.
        /// </summary>
        public int OpcodeCount
        {
            get
            {
                {
                    return VarConstOpcode + NumVar + NumConst;
                }
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="rnd"></param>
        /// <param name="parent"></param>
        /// <param name="current"></param>
        /// <param name="index"></param>
        /// <param name="reservoir"></param>
        private void InternalSampleRandomNode(IGenerateRandom rnd, TreeGenomeNode parent, TreeGenomeNode current,
            int[] index, RandomNodeResult reservoir)
        {
            int currentIndex = index[0];
            index[0]++;

            // determine if we replace the reservoir
            int j = rnd.NextInt(0, currentIndex + 1);
            if (j == 0)
            {
                reservoir.Parent = parent;
                reservoir.Child = current;
            }

            // traverse on to the children
            foreach (TreeGenomeNode child in current.Children)
            {
                InternalSampleRandomNode(rnd, current, child, index, reservoir);
            }
        }

        /// <summary>
        /// Choose a random node from the tree.  Uses reservoir sampling. 
        /// </summary>
        /// <param name="rnd">Random number generator.</param>
        /// <param name="root">The root of the tree.</param>
        /// <returns>A random node.</returns>
        public RandomNodeResult SampleRandomNode(IGenerateRandom rnd, TreeGenomeNode root)
        {
            var index = new int[1];
            var reservoir = new RandomNodeResult();
            index[0] = 0;
            InternalSampleRandomNode(rnd, null, root, index, reservoir);
            return reservoir;
        }

        /// <summary>
        /// Evaluate the specified node. 
        /// </summary>
        /// <param name="node">The node to evaluate.</param>
        /// <param name="varValues">The variable values.</param>
        /// <returns>The result of the evaluation.</returns>
        public abstract double Evaluate(TreeGenomeNode node, double[] varValues);

        /// <summary>
        /// Determine the number of children the specified opcode can have. 
        /// </summary>
        /// <param name="opcode">The opcode.</param>
        /// <returns>The number of children this opcode can have.</returns>
        public abstract int DetermineChildCount(int opcode);

        /// <summary>
        /// Choose a random opcode, choose between both leafs and nodes. 
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <returns>A random opcode.</returns>
        public int ChooseRandomOpcode(IGenerateRandom rnd)
        {
            return rnd.NextInt(0, OpcodeCount);
        }

        /// <summary>
        /// Choose a random opcode, choose between only leafs.
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <returns>A random opcode.</returns>
        public int ChooseRandomLeafOpcode(IGenerateRandom rnd)
        {
            return VarConstOpcode + rnd.NextInt(NumVar + NumConst);
        }

        /// <summary>
        /// Choose a random opcode, choose between only nodes. 
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <returns>A random opcode.</returns>
        public int ChooseRandomNodeOpcode(IGenerateRandom rnd)
        {
            return rnd.NextInt(VarConstOpcode);
        }

        /// <summary>
        /// Grow the tree randomly by the specified max depth. 
        /// </summary>
        /// <param name="rnd">A random number generator.</param>
        /// <param name="maxDepth">The max depth.</param>
        /// <returns>The tree.</returns>
        public TreeGenomeNode Grow(IGenerateRandom rnd, int maxDepth)
        {
            if (maxDepth == 1)
            {
                return new TreeGenomeNode(ChooseRandomLeafOpcode(rnd));
            }
            var result = new TreeGenomeNode(ChooseRandomNodeOpcode(rnd));
            int childCount = DetermineChildCount(result.Opcode);
            for (int i = 0; i < childCount; i++)
            {
                result.Children.Add(Grow(rnd, maxDepth - 1));
            }
            return result;
        }

        /// <summary>
        /// A set of leaf opcodes.
        /// </summary>
        /// <returns>A set of leaf opcodes.</returns>
        public HashSet<int> GetLeafSet()
        {
            var result = new HashSet<int>();
            for (int i = VarConstOpcode; i < OpcodeCount; i++)
            {
                result.Add(i);
            }
            return result;
        }

        /// <summary>
        /// A set of node opcodes.
        /// </summary>
        /// <returns>A set of node opcodes.</returns>
        public HashSet<int> GetNodeSet()
        {
            var result = new HashSet<int>();
            for (int i = 0; i < VarConstOpcode; i++)
            {
                result.Add(i);
            }
            return result;
        }
    }
}
