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

import java.util.ArrayList;
import java.util.List;

/**
 * A node in a Genetic Programming tree.
 */
public class TreeGenomeNode {

    /**
     * The child nodes.
     */
    private List<TreeGenomeNode> children = new ArrayList<TreeGenomeNode>();

    /**
     * The opcode for this node.
     */
    private int opcode;

    /**
     * Constructor.
     * @param theOpcode The opcode for this node.
     */
    public TreeGenomeNode(int theOpcode) {
        this.opcode = theOpcode;
    }

    /**
     * @return The opcode for this node.
     */
    public int getOpcode() {
        return this.opcode;
    }

    /**
     * @return The children from this node.
     */
    public List<TreeGenomeNode> getChildren() {
        return this.children;
    }

    /**
     * Create a copy of this node.
     * @return A copy (clone) of this node.
     */
    public TreeGenomeNode copy() {
        TreeGenomeNode result = new TreeGenomeNode(opcode);
        for (TreeGenomeNode child : this.children) {
            result.getChildren().add(child.copy());
        }
        return result;
    }

    /**
     * @return The size of this node.
     */
    public int size() {
        int result = 1;
        for (TreeGenomeNode child : this.children) {
            result += child.size();
        }
        return result;
    }

}
