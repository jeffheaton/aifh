package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/23/14
 * Time: 9:54 AM
 * To change this template use File | Settings | File Templates.
 */
public class PlantPhysics {

    private void distributeEnergy(PlantUniverse universe) {
        // Distribute sun energy downward
        double[] sunlight = new double[PlantUniverse.UNIVERSE_WIDTH];
        for(int i=0;i<sunlight.length;i++) {
            sunlight[i] = 1.0;
        }

        for(int row=0;row<PlantUniverse.UNIVERSE_HEIGHT;row++) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                double decay;

                // no sun underground
                if( row>=PlantUniverse.GROUND_LINE ) {
                    // blocked
                    decay = 0;
                } else {
                    // no decay until otherwise calculated
                    decay = 1;
                }

                PlantUniverseCell cell = universe.getCell(row,col);
                cell.setCalculatedSunlight(sunlight[col]);

                // Collect resources for live cells
                if( cell.isAlive() ) {
                    // Live cells cause the sunlight to decay (shade)
                    decay *= PlantUniverse.SUNLIGHT_DECAY * cell.getComposition();

                    // Set the energy based on sunlight level and composition of the live cell
                    double myEnergy = cell.getCalculatedSunlight()*cell.getComposition();
                    double transEnergy = universe.calculateTransferEnergy(row,col) * (1.0 - cell.getComposition());
                    double e = Math.max(myEnergy,transEnergy);
                    e = Math.max(PlantUniverse.MIN_LIVING_ENERGY,e);
                    cell.setEnergy(e);
                }

                sunlight[col]*=decay;

            }
        }
    }

    private void distributeNourishment(PlantUniverse universe) {
        int rootCount = 0;
        int surfaceCount = 0;

        // Distribute sun energy downward
        double[] waterTable = new double[PlantUniverse.UNIVERSE_WIDTH];
        for(int i=0;i<waterTable.length;i++) {
            waterTable[i] = 1.0;
        }

        for(int row=PlantUniverse.UNIVERSE_HEIGHT-1;row>=0;row--) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                double decay;

                // no water above ground
                if( row<PlantUniverse.GROUND_LINE ) {
                    // blocked
                    decay = 0;
                } else {
                    // no decay until otherwise calculated
                    decay = 1;
                }

                PlantUniverseCell cell = universe.getCell(row,col);
                cell.setCalculatedWater(waterTable[col]);

                // Collect resources for live cells
                if( cell.isAlive() ) {
                    // Live cells cause the water to decay (roots collect)
                    decay *= PlantUniverse.NOURISHMENT_DECAY;

                    // Set the energy based on sunlight level and composition of the live cell
                    double myWater = cell.getCalculatedWater()*cell.getComposition();
                    double transWater = universe.calculateTransferWater(row,col) * (1.0 - cell.getComposition());
                    double n = Math.max(myWater,transWater);
                    n = Math.max(PlantUniverse.MIN_LIVING_ENERGY,n);
                    cell.setNourishment(n);

                    // update the root and surface counts
                    if( row>=PlantUniverse.GROUND_LINE ) {
                        rootCount++;
                    } else {
                        if( cell.getComposition()>0.5 ) {
                            surfaceCount++;
                        }
                    }
                }

                waterTable[col]*=decay;

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
