package com.heatonresearch.aifh.genetic.trees;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/25/14
 * Time: 7:07 AM
 * To change this template use File | Settings | File Templates.
 */
public class TreeGenomeNode {
    private List<TreeGenomeNode> children = new ArrayList<TreeGenomeNode>();
    private int opcode;

    public TreeGenomeNode(int theOpcode) {
        this.opcode = theOpcode;
    }

    public int getOpcode() {
        return this.opcode;
    }

    public List<TreeGenomeNode> getChildren() {
        return this.children;
    }

    public TreeGenomeNode copy() {
        TreeGenomeNode result = new TreeGenomeNode(opcode);
        for(TreeGenomeNode child: this.children) {
            result.getChildren().add(child.copy());
        }
        return result;
    }

    public int size() {
        int result = 1;
        for(TreeGenomeNode child: this.children) {
            result+=child.size();
        }
        return result;
    }

}
