package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.evolutionary.genome.BasicGenome;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/25/14
 * Time: 7:07 AM
 * To change this template use File | Settings | File Templates.
 */
public class TreeGenome extends BasicGenome implements RegressionAlgorithm {

    private EvaluateTree evaluator;
    private TreeGenomeNode root;

    public TreeGenome(EvaluateTree theEvaluator) {
        this.evaluator = theEvaluator;
    }

    @Override
    public void copy(final Genome source) {
        this.root = ((TreeGenome)source).getRoot().copy();
    }

    @Override
    public int size() {
        return root.size();
    }

    @Override
    public double[] getLongTermMemory() {
        throw new AIFHError("Long term memory not supported, use a genetic trainer.");
    }

    public TreeGenomeNode getRoot() {
        return this.root;
    }

    public void setRoot(TreeGenomeNode node) {
        this.root = node;
    }

    public EvaluateTree getEvaluator() {
        return this.evaluator;
    }

    @Override
    public double[] computeRegression(final double[] input) {
        double[] result = new double[1];
        result[0] =  evaluator.evaluate(this.root,input);
        return result;
    }
}
