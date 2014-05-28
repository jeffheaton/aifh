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
package com.heatonresearch.aifh.examples.capstone.alife.milestone3;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * This class is used to score the plant.  Plants are scored for how green they are after a specified
 * number of iterations.
 */
public class PlantScore implements ScoreFunction {

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateScore(final MLMethod algo) {
        DoubleArrayGenome genome = (DoubleArrayGenome) algo;
        PlantUniverse universe = new PlantUniverse();
        universe.reset();
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        // Run the generations.
        for (int i = 0; i < PlantUniverse.EVALUATION_CYCLES; i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe, genome.getData());
        }

        // Count the amount of green.
        int count = 0;
        double sum = 0;
        for (int row = 0; row < PlantUniverse.UNIVERSE_HEIGHT; row++) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                PlantUniverseCell cell = universe.getCell(row, col);
                if (cell.isAlive()) {
                    if (row >= PlantUniverse.GROUND_LINE) {
                        sum += 0.5;
                    } else {
                        sum += cell.getLeafyness();
                    }
                }
                count++;
            }
        }
        return sum / count;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean shouldMinimize() {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
