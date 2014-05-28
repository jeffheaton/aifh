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
package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.HashSet;
import java.util.Set;

/**
 * Evaluate a tree.  Used for genetic programming.
 *
 * References:
 *
 * http://en.wikipedia.org/wiki/Reservoir_sampling
 *
 */
public abstract class EvaluateTree {

    /**
     * Choose a random tree node.  Uses reservoir sampling.
     * @param rnd A random number generator.
     * @param parent The parent node.
     * @param current The current node in our traversal.
     * @param index The current index, it is an array for pass by reference.
     * @param reservoir The reservoir.
     */
    private void internalSampleRandomNode(GenerateRandom rnd, TreeGenomeNode parent, TreeGenomeNode current, int[] index, RandomNodeResult reservoir) {
        int currentIndex = index[0];
        index[0]++;

        // determine if we replace the reservoir
        int j = rnd.nextInt(0, currentIndex + 1);
        if (j == 0) {
            reservoir.setParent(parent);
            reservoir.setChild(current);
        }

        // traverse on to the children
        for (TreeGenomeNode child : current.getChildren()) {
            internalSampleRandomNode(rnd, current, child, index, reservoir);
        }
    }

    /**
     * Choose a random node from the tree.  Uses reservoir sampling.
     * @param rnd Random number generator.
     * @param root The root of the tree.
     * @return A random node.
     */
    public RandomNodeResult sampleRandomNode(GenerateRandom rnd, TreeGenomeNode root) {
        int[] index = new int[1];
        RandomNodeResult reservoir = new RandomNodeResult();
        index[0] = 0;
        internalSampleRandomNode(rnd, null, root, index, reservoir);
        return reservoir;
    }

    /**
     * @return The first opcode for the variable and constant nodes.
     */
    public abstract int getVarConstOpcode();

    /**
     * @return The number of constants supported.
     */
    public abstract int getNumConst();

    /**
     * @return The number of variables supported.
     */
    public abstract int getNumVar();

    /**
     * Evaluate the specified node.
     * @param node The node to evaluate.
     * @param varValues The variable values.
     * @return The result of the evaluation.
     */
    public abstract double evaluate(TreeGenomeNode node, double[] varValues);

    /**
     * Determine the number of children the specified opcode can have.
     * @param opcode The opcode.
     * @return The number of children this opcode can have.
     */
    public abstract int determineChildCount(int opcode);

    /**
     * @return The total number of opcodes.
     */
    public int opcodeCount() {
        return getVarConstOpcode() + getNumVar() + getNumConst();
    }

    /**
     * Choose a random opcode, choose between both leafs and nodes.
     * @param rnd A random number generator.
     * @return A random opcode.
     */
    public int chooseRandomOpcode(GenerateRandom rnd) {
        return rnd.nextInt(0, opcodeCount());
    }

    /**
     * Choose a random opcode, choose between only leafs.
     * @param rnd A random number generator.
     * @return A random opcode.
     */
    public int chooseRandomLeafOpcode(GenerateRandom rnd) {
        return getVarConstOpcode() + rnd.nextInt(getNumVar() + getNumConst());
    }

    /**
     * Choose a random opcode, choose between only nodes.
     * @param rnd A random number generator.
     * @return A random opcode.
     */
    public int chooseRandomNodeOpcode(GenerateRandom rnd) {
        return rnd.nextInt(getVarConstOpcode());
    }

    /**
     * Grow the tree randomly by the specified max depth.
     * @param rnd A random number generator.
     * @param maxDepth The max depth.
     * @return The tree.
     */
    public TreeGenomeNode grow(GenerateRandom rnd, int maxDepth) {
        if (maxDepth == 1) {
            return new TreeGenomeNode(chooseRandomLeafOpcode(rnd));
        } else {
            TreeGenomeNode result = new TreeGenomeNode(chooseRandomNodeOpcode(rnd));
            int childCount = determineChildCount(result.getOpcode());
            for (int i = 0; i < childCount; i++) {
                result.getChildren().add(grow(rnd, maxDepth - 1));
            }
            return result;
        }
    }

    /**
     * @return A set of leaf opcodes.
     */
    public Set<Integer> getLeafSet() {
        Set<Integer> result = new HashSet<Integer>();
        for (int i = this.getVarConstOpcode(); i < opcodeCount(); i++) {
            result.add(i);
        }
        return result;
    }

    /**
     * @return A set of node opcodes.
     */
    public Set<Integer> getNodeSet() {
        Set<Integer> result = new HashSet<Integer>();
        for (int i = 0; i < this.getVarConstOpcode(); i++) {
            result.add(i);
        }
        return result;
    }


}
