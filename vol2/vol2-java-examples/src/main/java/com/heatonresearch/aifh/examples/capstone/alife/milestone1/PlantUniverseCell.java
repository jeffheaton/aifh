package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

import com.heatonresearch.aifh.AIFH;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/20/14
 * Time: 6:01 AM
 * To change this template use File | Settings | File Templates.
 */
public class PlantUniverseCell {
    /**
     * How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
     */
    private double composition = 0;
    private double energy = 0;
    private double nourishment = 0;

    private double calculatedSunlight = 0;
    private double calculatedWater = 0;

    public boolean isAlive() {
        return this.energy>AIFH.DEFAULT_PRECISION;
    }

    public double getComposition() {
        return composition;
    }

    public void setComposition(final double composition) {
        this.composition = composition;
    }

    public double getEnergy() {
        return energy;
    }

    public void setEnergy(final double energy) {
        this.energy = energy;
    }

    public double getNourishment() {
        return nourishment;
    }

    public void setNourishment(final double nourishment) {
        this.nourishment = nourishment;
    }

    public double getCalculatedSunlight() {
        return calculatedSunlight;
    }

    public void setCalculatedSunlight(final double calculatedSunlight) {
        this.calculatedSunlight = calculatedSunlight;
    }

    public double getCalculatedWater() {
        return calculatedWater;
    }

    public void setCalculatedWater(final double calculatedWater) {
        this.calculatedWater = calculatedWater;
    }
}
