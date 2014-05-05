package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.genetic.trees.TreeGenomeNode;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/5/14
 * Time: 10:41 AM
 * To change this template use File | Settings | File Templates.
 */
public class RandomNodeResult {
    private TreeGenomeNode parent;
    private TreeGenomeNode child;

    public TreeGenomeNode getParent() {
        return parent;
    }

    public void setParent(final TreeGenomeNode parent) {
        this.parent = parent;
    }

    public TreeGenomeNode getChild() {
        return child;
    }

    public void setChild(final TreeGenomeNode child) {
        this.child = child;
    }
}
