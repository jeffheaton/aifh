package com.heatonresearch.aifh.examples.capstone.alife.milestone2;

import com.heatonresearch.aifh.distance.EuclideanDistance;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverse;
import com.heatonresearch.aifh.examples.capstone.alife.milestone1.PlantUniverseCell;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/23/14
 * Time: 4:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class PlantGrowth {
    private final int[] colTransform = {0, 0, -1, 1, -1, 1, 1, -1};
    private final int[] rowTransform = {-1, 1, 0, 0, -1, 1, -1, 1};

    private final EuclideanDistance dist = new EuclideanDistance();
    private final boolean[][] newComposition = new boolean[PlantUniverse.UNIVERSE_HEIGHT][PlantUniverse.UNIVERSE_WIDTH];

    private double getGrowthPotential(PlantUniverse universe, int row, int col, double[] genome) {
        double[] cellVec = universe.getCellInfoVector(row,col);
        double d1 = dist.calculate(cellVec,0,genome,PlantUniverse.CELL_VECTOR_LENGTH*2,PlantUniverse.CELL_VECTOR_LENGTH);
        double d2 = dist.calculate(cellVec,0,genome,PlantUniverse.CELL_VECTOR_LENGTH*3,PlantUniverse.CELL_VECTOR_LENGTH);

        double result = Math.min(d1,d2);
        if( result<PlantUniverse.MIN_GROWTH_DIST ) {
            result = -1;
        }

        return result;

    }

    public void runGrowth(PlantUniverse universe, double[] genome) {
        // Does this plant have enough roots to grow?
        if( universe.getSurfaceCount()==0 ) {
            return;
        }
        double rootRatio = (double)universe.getRootCount()/(double)universe.getSurfaceCount();
        if( rootRatio < PlantUniverse.REQUIRED_ROOT_RATIO ) {
            return;
        }


        // Reset the new composition to be the composition of the current universe
        for(int row=0;row<PlantUniverse.UNIVERSE_HEIGHT;row++) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                PlantUniverseCell cell = universe.getCell(row,col);
                this.newComposition[row][col] = false;
            }
        }

        for(int row=0;row<PlantUniverse.UNIVERSE_HEIGHT;row++) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                PlantUniverseCell cell = universe.getCell(row,col);

                // see if we want to change the composition
                if( row<PlantUniverse.GROUND_LINE ) {
                    double[] cellVec = universe.getCellInfoVector(row,col);
                    double d1 = dist.calculate(cellVec,0,genome,0,PlantUniverse.CELL_VECTOR_LENGTH);
                    double d2 = dist.calculate(cellVec,0,genome,PlantUniverse.CELL_VECTOR_LENGTH,PlantUniverse.CELL_VECTOR_LENGTH);

                    if( d1<d2 ) {
                        cell.setComposition(cell.getComposition() * PlantUniverse.STEM_TRANSITION);
                    }
                }

                // Evaluate growth into each neighbor cell
                if( universe.canGrow(row,col) ) {
                    int growthTargetRow = row;
                    int growthTargetCol = col;
                    double growthTargetScore = Double.POSITIVE_INFINITY;

                    for(int i=0;i<colTransform.length;i++) {
                        int evalCol = col + colTransform[i];
                        int evalRow = row + rowTransform[i];
                        if( universe.isValid(evalRow,evalCol) ) {
                            double p = getGrowthPotential(universe,evalRow,evalCol,genome);
                            if( p>0 ) {
                                if( p<growthTargetScore ) {
                                    growthTargetScore = p;
                                    growthTargetRow = evalRow;
                                    growthTargetCol = evalCol;
                                }
                            }
                        }
                    }

                    // Grow new cell, if requested, did we ever set target row & col to anything?
                    if( growthTargetRow!=row || growthTargetCol!=col ) {
                        this.newComposition[growthTargetRow][growthTargetCol] = true;
                    }
                }
            }
        }

        // Copy the new composition back to the universe
        for(int row=0;row<PlantUniverse.UNIVERSE_HEIGHT;row++) {
            for(int col=0;col<PlantUniverse.UNIVERSE_WIDTH;col++) {
                PlantUniverseCell cell = universe.getCell(row,col);




                if( this.newComposition[row][col]  ) {
                    if( row>=PlantUniverse.GROUND_LINE ) {
                        // Roots are always 100% stem for transfer.
                        cell.setComposition(0);
                    } else {
                        cell.setComposition(1.0);
                    }
                    cell.setEnergy(1.0);
                    cell.setNourishment(1.0);
                }
            }
        }
    }
}
