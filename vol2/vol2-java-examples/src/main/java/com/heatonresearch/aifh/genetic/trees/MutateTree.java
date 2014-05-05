package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.opp.EvolutionaryOperator;
import com.heatonresearch.aifh.evolutionary.train.EvolutionaryAlgorithm;
import com.heatonresearch.aifh.randomize.GenerateRandom;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/5/14
 * Time: 10:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class MutateTree implements EvolutionaryOperator {
    private EvolutionaryAlgorithm owner;
    private int maxGraftLength;

    public MutateTree(int theMaxGraftLength) {
        this.maxGraftLength = Math.max(1,theMaxGraftLength);
    }

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
        EvaluateTree eval = parent1.getEvaluator();
        TreeGenome off1 = (TreeGenome)this.owner.getPopulation().getGenomeFactory().factor(parent1);
        RandomNodeResult off1Point = eval.sampleRandomNode(rnd,off1.getRoot());

        int len = rnd.nextInt(1,this.maxGraftLength+1);
        TreeGenomeNode randomSequence = eval.grow(rnd,len);

        if(off1Point.getParent()==null) {
            off1.setRoot(randomSequence);
        } else {
            int idx = off1Point.getParent().getChildren().indexOf(off1Point.getChild());
            off1Point.getParent().getChildren().set(idx,randomSequence);
        }

        offspring[0] = off1;
    }
}
