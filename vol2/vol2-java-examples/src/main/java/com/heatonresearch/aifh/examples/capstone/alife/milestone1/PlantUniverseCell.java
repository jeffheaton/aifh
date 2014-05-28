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
package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

import com.heatonresearch.aifh.AIFH;

/**
 * An individual cell in the plant universe.
 */
public class PlantUniverseCell {
    /**
     * How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
     */
    private double leafyness = 0;

    /**
     * The amount of energy between [0,1].
     */
    private double energy = 0;

    /**
     * The amount of nourishment between [0,1].
     */
    private double nourishment = 0;

    /**
     * The calculated sunlight exposure.
     */
    private double calculatedSunlight = 0;

    /**
     * The calculated water exposure.
     */
    private double calculatedWater = 0;

    /**
     * @return True, if this cell is alive.
     */
    public boolean isAlive() {
        return this.energy > AIFH.DEFAULT_PRECISION;
    }

    /**
     * @return The degree of leafyness for this cell.
     */
    public double getLeafyness() {
        return leafyness;
    }

    /**
     * Set the leafyness for the cell.
     *
     * @param leafyness How leafy the cell is.
     */
    public void setLeafyness(final double leafyness) {
        this.leafyness = leafyness;
    }

    /**
     * @return The energy in this cell [0,1].
     */
    public double getEnergy() {
        return energy;
    }

    /**
     * Set the energy
     *
     * @param energy The energy.
     */
    public void setEnergy(final double energy) {
        this.energy = energy;
    }

    /**
     * @return The nourishment for this cell.
     */
    public double getNourishment() {
        return nourishment;
    }

    /**
     * Set the nourishment for a cell.
     *
     * @param nourishment The nourishment.
     */
    public void setNourishment(final double nourishment) {
        this.nourishment = nourishment;
    }

    /**
     * @return The calculated sunlight for this cell.
     */
    public double getCalculatedSunlight() {
        return calculatedSunlight;
    }

    /**
     * Set the calculated sunlight for a cell.
     *
     * @param calculatedSunlight The calculated sunlight.
     */
    public void setCalculatedSunlight(final double calculatedSunlight) {
        this.calculatedSunlight = calculatedSunlight;
    }

    /**
     * @return The calculated water for this cell.
     */
    public double getCalculatedWater() {
        return calculatedWater;
    }

    /**
     * Set the calculated water for a cell.
     *
     * @param calculatedWater The calculated water.
     */
    public void setCalculatedWater(final double calculatedWater) {
        this.calculatedWater = calculatedWater;
    }
}
