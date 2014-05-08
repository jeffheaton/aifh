package com.heatonresearch.aifh.genetic.species;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.evolutionary.species.ThresholdSpeciation;
import com.heatonresearch.aifh.genetic.genome.ArrayGenome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;

public class ArraySpeciation<T extends ArrayGenome> extends ThresholdSpeciation {
    @Override
    public double getCompatibilityScore(final Genome genome1, final Genome genome2) {
        if( genome1 instanceof DoubleArrayGenome) {
            return scoreDouble(genome1, genome2);
        } else if( genome1 instanceof IntegerArrayGenome ) {
            return scoreInt(genome1,genome2);
        } else {
            throw new AIFHError("This speciation does not support: " + genome1.getClass().getName());
        }
    }

    private double scoreInt(final Genome genome1, final Genome genome2) {
        IntegerArrayGenome intGenome1 = (IntegerArrayGenome)genome1;
        IntegerArrayGenome intGenome2 = (IntegerArrayGenome)genome2;
        double sum = 0;
        for(int i=0;i<intGenome1.size();i++) {
            double diff = intGenome1.getData()[i] - intGenome2.getData()[i];
            sum+=diff*diff;
        }
        return Math.sqrt(sum);
    }

    private double scoreDouble(final Genome genome1, final Genome genome2) {
        DoubleArrayGenome doubleGenome1 = (DoubleArrayGenome)genome1;
        DoubleArrayGenome doubleGenome2 = (DoubleArrayGenome)genome2;
        double sum = 0;
        for(int i=0;i<doubleGenome1.size();i++) {
            double diff = doubleGenome1.getData()[i] - doubleGenome2.getData()[i];
            sum+=diff*diff;
        }
        return Math.sqrt(sum);
    }
}
