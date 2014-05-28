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
        if (genome1 instanceof DoubleArrayGenome) {
            return scoreDouble(genome1, genome2);
        } else if (genome1 instanceof IntegerArrayGenome) {
            return scoreInt(genome1, genome2);
        } else {
            throw new AIFHError("This speciation does not support: " + genome1.getClass().getName());
        }
    }

    private double scoreInt(final Genome genome1, final Genome genome2) {
        IntegerArrayGenome intGenome1 = (IntegerArrayGenome) genome1;
        IntegerArrayGenome intGenome2 = (IntegerArrayGenome) genome2;
        double sum = 0;
        for (int i = 0; i < intGenome1.size(); i++) {
            double diff = intGenome1.getData()[i] - intGenome2.getData()[i];
            sum += diff * diff;
        }
        return Math.sqrt(sum);
    }

    private double scoreDouble(final Genome genome1, final Genome genome2) {
        DoubleArrayGenome doubleGenome1 = (DoubleArrayGenome) genome1;
        DoubleArrayGenome doubleGenome2 = (DoubleArrayGenome) genome2;
        double sum = 0;
        for (int i = 0; i < doubleGenome1.size(); i++) {
            double diff = doubleGenome1.getData()[i] - doubleGenome2.getData()[i];
            sum += diff * diff;
        }
        return Math.sqrt(sum);
    }
}
