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
package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;

/**
 * The physics class defines limits on the growth that the genome wants to implement.  Specifically, the physics
 * determines how sunlight is absorbed and nourishment is distributed in the plant.
 * <p/>
 * Sunlight comes from above and stops at the ground level. More leafy material absorbs sunlight and reduces it
 * because of shade.  Water comes from below and is absorbed by the roots.
 */
public class PlantPhysics {

    /**
     * Distribute the sunlight energy in the universe.
     *
     * @param universe The universe.
     */
    private void distributeEnergy(PlantUniverse universe) {
        // Distribute sun energy downward
        double[] sunlight = new double[PlantUniverse.UNIVERSE_WIDTH];
        for (int i = 0; i < sunlight.length; i++) {
            sunlight[i] = 1.0;
        }

        for (int row = 0; row < PlantUniverse.UNIVERSE_HEIGHT; row++) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                double decay;

                // no sun underground
                if (row >= PlantUniverse.GROUND_LINE) {
                    // blocked
                    decay = 0;
                } else {
                    // no decay until otherwise calculated
                    decay = 1;
                }

                PlantUniverseCell cell = universe.getCell(row, col);
                cell.setCalculatedSunlight(sunlight[col]);

                // Collect resources for live cells
                if (cell.isAlive()) {
                    // Live cells cause the sunlight to decay (shade)
                    decay *= PlantUniverse.DECAY * cell.getLeafyness();

                    // Set the energy based on sunlight level and composition of the live cell
                    double myEnergy = cell.getCalculatedSunlight() * cell.getLeafyness();
                    double transEnergy = universe.calculateTransferEnergy(row, col) * (1.0 - cell.getLeafyness());
                    double e = Math.max(myEnergy, transEnergy);
                    e = Math.max(PlantUniverse.MIN_LIVING_ENERGY, e);
                    cell.setEnergy(e);
                }

                sunlight[col] *= decay;

            }
        }
    }

    /**
     * Distribute nourishment in the universe.
     *
     * @param universe The universe.
     */
    private void distributeNourishment(PlantUniverse universe) {
        double rootCount = 0;
        double surfaceCount = 0;

        // Distribute sun energy downward
        double[] waterTable = new double[PlantUniverse.UNIVERSE_WIDTH];
        for (int i = 0; i < waterTable.length; i++) {
            waterTable[i] = 1.0;
        }

        for (int row = PlantUniverse.UNIVERSE_HEIGHT - 1; row >= 0; row--) {
            for (int col = 0; col < PlantUniverse.UNIVERSE_WIDTH; col++) {
                double decay;

                // no water above ground
                if (row < PlantUniverse.GROUND_LINE) {
                    // blocked
                    decay = 0;
                } else {
                    // no decay until otherwise calculated
                    decay = 1;
                }

                PlantUniverseCell cell = universe.getCell(row, col);
                cell.setCalculatedWater(waterTable[col]);

                // Collect resources for live cells
                if (cell.isAlive()) {
                    // Live cells cause the water to decay (roots collect)
                    decay *= PlantUniverse.DECAY;

                    // Set the energy based on sunlight level and composition of the live cell
                    double myWater = cell.getCalculatedWater() * cell.getLeafyness();
                    double transWater = universe.calculateTransferNourishment(row, col) * (1.0 - cell.getLeafyness());
                    double n = Math.max(myWater, transWater);
                    n = Math.max(PlantUniverse.MIN_LIVING_ENERGY, n);
                    cell.setNourishment(n);

                    // update the root and surface counts
                    if (row >= PlantUniverse.GROUND_LINE) {
                        rootCount += cell.getNourishment();
                    } else {
                        surfaceCount += cell.getLeafyness();
                    }
                }

                waterTable[col] *= decay;

            }
        }

        universe.setRootCount(rootCount);
        universe.setSurfaceCount(surfaceCount);
    }


    public void runPhysics(PlantUniverse universe) {
        distributeEnergy(universe);
        distributeNourishment(universe);


    }
}
