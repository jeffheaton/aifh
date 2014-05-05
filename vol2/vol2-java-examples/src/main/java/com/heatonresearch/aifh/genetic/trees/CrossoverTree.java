package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/5/14
 * Time: 10:31 AM
 * To change this template use File | Settings | File Templates.
 */
public class CrossoverTree implements EvolutionaryOperator {
    private EvolutionaryAlgorithm owner;

    @Override
    public void init(final EvolutionaryAlgorithm theOwner) {
        this.owner = theOwner;
    }

    @Override
    public int offspringProduced() {
        return 1;
    }

    @Override
    public int parentsNeeded() {
        return 1;
    }

    @Override
    public void performOperation(final GenerateRandom rnd, final Genome[] parents, final int parentIndex, final Genome[] offspring, final int offspringIndex) {
        TreeGenome parent1 = (TreeGenome)parents[parentIndex];
        TreeGenome parent2 = (TreeGenome)parents[parentIndex];
        EvaluateTree eval = parent1.getEvaluator();

        TreeGenome off1 = (TreeGenome)this.owner.getPopulation().getGenomeFactory().factor(parent1);
        RandomNodeResult replacePoint = eval.sampleRandomNode(rnd,off1.getRoot());
        RandomNodeResult copySource = eval.sampleRandomNode(rnd,parent2.getRoot());
        TreeGenomeNode actualCopy = copySource.getChild().copy();

        if(replacePoint.getParent()==null) {
            off1.setRoot(actualCopy);
        } else {
            int idx = replacePoint.getParent().getChildren().indexOf(replacePoint.getChild());
            replacePoint.getParent().getChildren().set(idx,actualCopy);
        }

        offspring[0] = off1;
    }
}
