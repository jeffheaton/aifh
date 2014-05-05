package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/5/14
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class EvaluateTree {

    private void internalSampleRandomNode(GenerateRandom rnd, TreeGenomeNode parent, TreeGenomeNode current, int[] index, RandomNodeResult reservoir) {
        int currentIndex = index[0];
        index[0]++;

        // determine if we replace the reservoir
        int j = rnd.nextInt(0,currentIndex+1);
        if( j==0 ) {
            reservoir.setParent(parent);
            reservoir.setChild(current);
        }

        // traverse on to the children
        for(TreeGenomeNode child: current.getChildren()) {
            internalSampleRandomNode(rnd,current,child,index,reservoir);
        }
    }
    public RandomNodeResult sampleRandomNode(GenerateRandom rnd, TreeGenomeNode root) {
        int[] index = new int[1];
        RandomNodeResult reservoir = new RandomNodeResult();
        index[0] = 0;
        internalSampleRandomNode(rnd,null,root,index,reservoir);
        return reservoir;
    }

    public abstract int getVarConstOpcode();
    public abstract int getNumConst();
    public abstract int getNumVar();
    public abstract double evaluate(TreeGenomeNode node, double[] varValues);
    public abstract int determineChildCount(int opcode);

    public int opcodeCount() {
        return getVarConstOpcode() + getNumVar() + getNumConst();
    }

    public int chooseRandomOpcode(GenerateRandom rnd) {
        return rnd.nextInt(0,opcodeCount());
    }

    public int chooseRandomLeafOpcode(GenerateRandom rnd) {
        return getVarConstOpcode() + rnd.nextInt(getNumVar()+getNumConst());
    }

    public int chooseRandomNodeOpcode(GenerateRandom rnd) {
        return rnd.nextInt(getVarConstOpcode());
    }

    public TreeGenomeNode grow(GenerateRandom rnd, int maxDepth) {
        if( maxDepth==1 ) {
            return new TreeGenomeNode(chooseRandomLeafOpcode(rnd));
        } else {
            TreeGenomeNode result = new TreeGenomeNode(chooseRandomNodeOpcode(rnd));
            int childCount = determineChildCount(result.getOpcode());
            for(int i=0;i<childCount;i++) {
                result.getChildren().add(grow(rnd,maxDepth-1));
            }
            return result;
        }
    }
}
