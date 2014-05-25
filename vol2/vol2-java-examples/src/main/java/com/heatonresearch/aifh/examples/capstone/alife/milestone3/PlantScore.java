package com.heatonresearch.aifh.examples.capstone.alife.milestone3;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantGrowth;
import com.heatonresearch.aifh.examples.capstone.alife.milestone2.PlantPhysics;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.score.ScoreFunction;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/24/14
 * Time: 6:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class PlantScore implements ScoreFunction {

    public static final int GENERATIONS = 100;

    @Override
    public double calculateScore(final MLMethod algo) {
        DoubleArrayGenome genome = (DoubleArrayGenome)algo;
        PlantUniverse universe = new PlantUniverse();
        universe.reset();
        PlantPhysics physics = new PlantPhysics();
        PlantGrowth growth = new PlantGrowth();

        // Run the generations.
        for(int i=0;i<GENERATIONS;i++) {
            physics.runPhysics(universe);
            growth.runGrowth(universe,genome.getData());
        }

        // Count the amount of green.
        int count = 0;
        double sum = 0;
        for(int row=0;row<PlantUniverse.GROUND_LINE;row++) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                PlantUniverseCell cell = universe.getCell(row,col);
                if( cell.isAlive() ) {
                    sum+=cell.getComposition();
                }
                count++;
            }
        }
        return sum/count;

    }

    @Override
    public boolean shouldMinimize() {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
