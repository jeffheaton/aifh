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
        return this.energy>AIFH.DEFAULT_PRECISION;
    }

    /**
     * @return The degree of leafyness for this cell.
     */
    public double getLeafyness() {
        return leafyness;
    }

    /**
     * Set the leafyness for the cell.
     * @param leafyness
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
     * @param energy
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
     * @param calculatedWater
     */
    public void setCalculatedWater(final double calculatedWater) {
        this.calculatedWater = calculatedWater;
    }
}
