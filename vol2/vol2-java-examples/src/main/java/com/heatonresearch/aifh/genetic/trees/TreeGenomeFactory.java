package com.heatonresearch.aifh.genetic.trees;

import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.genome.GenomeFactory;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/4/14
 * Time: 3:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class TreeGenomeFactory implements GenomeFactory {

    private EvaluateTree eval;

    public TreeGenomeFactory(EvaluateTree theEval) {
        this.eval = theEval;
    }

    @Override
    public Genome factor() {
        return new TreeGenome(this.eval);
    }

    @Override
    public Genome factor(final Genome other) {
        TreeGenome result = new TreeGenome(this.eval);
        result.copy(other);
        return result;
    }
}
