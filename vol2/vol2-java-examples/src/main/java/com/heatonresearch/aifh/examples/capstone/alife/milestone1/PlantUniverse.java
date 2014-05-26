package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/20/14
 * Time: 6:00 AM
 * To change this template use File | Settings | File Templates.
 */
public class PlantUniverse {
    /**
     * The width of the universe, in terms of cells.
     * Any actual "on screen" display is scaled from this.
     */
    public static final int UNIVERSE_WIDTH = 50;

    /**
     * The height of the universe, in terms of cells.
     * Any actual "on screen" display is scaled from this.
     */
    public static final int UNIVERSE_HEIGHT = 100;

    /**
     * The location of the ground line.  Anything >= to this is underground.
     */
    public static final int GROUND_LINE = UNIVERSE_HEIGHT-(UNIVERSE_HEIGHT /3);

    /**
     * The size of a cell "info vector".  This vector identifies a cell's state, and is used to encode instructions
     * in the genome.  All of these are normalized to [0,1].  There are currently four elements:
     * 0: The row that the cell is in.
     * 1: The amount of sunlight the cell is exposed to.
     * 2: The degree of crowding, from neighbors.
     * 3: The amount of nourishment the cell is exposed to.
     */
    public static final int CELL_VECTOR_LENGTH = 4;

    /**
     * The size of a GENOME vector.  A genome vector is made up of four "info vectors".  These give instructions
     * on how to grow a plant.
     * Vector 0: Growth template #1
     * Vector 1: Growth template #2
     * Vector 2: Leaf template
     * Vector 3: Stem template
     * For more information on how these are used, refer to the PlantGrowth class.
     */
    public static final int GENOME_SIZE = CELL_VECTOR_LENGTH * 4;

    /**
     * The rate that sunlight decays based on shade, or
     * the rate that nourishment decays based on roots absorbing.
     */
    public static final double DECAY = 0.1;

    /**
     * The rate at which leafy material turns to wooden stem.
     */
    public static final double STEM_TRANSITION = 0.8;

    /**
     * The threshold to allow growth.
     */
    public static final double GROWTH_THRESHOLD = 0.25;


    /**
     * The minimum distance to a genome template to execute that instruction.
     * Used to control how growth happens.
     */
    public static final double MIN_GROWTH_DIST = 0.1;

    /**
     * The minimum energy that a living cell is allowed to drop to.
     */
    public static final double MIN_LIVING_ENERGY = 0.1;

    /**
     * The population size for the genetic algorithm.
     */
    public static final int POPULATION_SIZE = 1000;


    private PlantUniverseCell[][] grid = new PlantUniverseCell[UNIVERSE_HEIGHT][UNIVERSE_WIDTH];
    private double rootCount;
    private double surfaceCount;

    public PlantUniverse() {
        for(int row=0;row<grid.length;row++) {
            for(int col=0;col<grid[row].length;col++) {
                this.grid[row][col] = new PlantUniverseCell();
            }
        }
    }

    public PlantUniverseCell getCell(int row, int col) {
        return this.grid[row][col];
    }

    public double calculateCrowd(int row, int col) {
        if(!isValid(row,col))
            return 0;

        PlantUniverseCell cell = getCell(row, col);

        if( !cell.isAlive() ) {
            return 0;
        }

        return cell.getComposition();
    }

    public double calculateNourishment(int row, int col) {
        if(!isValid(row,col))
            return 0;

        PlantUniverseCell cell = getCell(row, col);

        if( !cell.isAlive() ) {
            return 0;
        }

        return cell.getNourishment();
    }

    public double calculateMeanNeighborsCrowd(int row, int col) {
        double sum = 0;
        sum+=calculateCrowd(row - 1, col - 1);
        sum+=calculateCrowd(row - 1, col);
        sum+=calculateCrowd(row - 1, col + 1);

        sum+=calculateCrowd(row, col - 1);
        sum+=calculateCrowd(row, col + 1);

        sum+=calculateCrowd(row + 1, col - 1);
        sum+=calculateCrowd(row + 1, col);
        sum+=calculateCrowd(row + 1, col + 1);

        return sum/8.0;
    }

    public double[] getCellInfoVector(int row, int col) {
        double[] result = new double[CELL_VECTOR_LENGTH];
        PlantUniverseCell cell = getCell(row,col);

        // Height
        result[0] = row/PlantUniverse.UNIVERSE_HEIGHT;
        // Sunlight
        if( row<PlantUniverse.GROUND_LINE) {
            result[1] = cell.getCalculatedSunlight();
        } else {
            result[1] = cell.getCalculatedWater();
        }
        // Crowd
        result[2] = calculateMeanNeighborsCrowd(row,col);
        // Nourishment
        result[3] = cell.getNourishment();
        //


        return result;
    }

    public void reset() {
        for(int row=0;row<grid.length;row++) {
            for(int col=0;col<grid[row].length;col++) {
                PlantUniverseCell cell = this.grid[row][col];
                cell.setComposition(0);
                cell.setEnergy(0);
                cell.setNourishment(0);
            }
        }

        int center = PlantUniverse.UNIVERSE_WIDTH/2;
        int groundLevel = PlantUniverse.GROUND_LINE;

        // root
        grid[groundLevel][center].setComposition(0);
        grid[groundLevel][center].setNourishment(1);
        grid[groundLevel][center].setEnergy(1);

        // stem
        grid[groundLevel-1][center].setComposition(0.5);
        grid[groundLevel-1][center].setNourishment(1);
        grid[groundLevel-1][center].setEnergy(1);

        // leaf
        grid[groundLevel-2][center].setComposition(1);
        grid[groundLevel-2][center].setNourishment(1);
        grid[groundLevel-2][center].setEnergy(1);

    }

    public boolean isValid(final int row, final int col) {
        if( row<0 || col<0 ) {
            return false;
        }

        if( row>=this.grid.length ) {
            return false;
        }

        if( col>=this.grid[row].length ) {
            return false;
        }

        return true;
    }

    public double calculateEnergy(final int row, final int col) {
        if( !isValid(row,col) ) {
            return 0;
        }
        return this.grid[row][col].getEnergy();
    }

    public double calculateTransferEnergy(final int row, final int col) {
        double result = 0;
        result=Math.max(result, calculateEnergy(row - 1, col - 1));
        result=Math.max(result, calculateEnergy(row - 1, col));
        result=Math.max(result, calculateEnergy(row - 1, col + 1));
        return result;
    }


    public double calculateTransferWater(final int row, final int col) {
        double result = 0;
        result=Math.max(result, calculateEnergy(row + 1, col - 1));
        result=Math.max(result, calculateEnergy(row + 1, col));
        result=Math.max(result, calculateEnergy(row + 1, col + 1));
        return result;
    }

    public double getRootCount() {
        return rootCount;
    }

    public void setRootCount(final double rootCount) {
        this.rootCount = rootCount;
    }

    public double getSurfaceCount() {
        return surfaceCount;
    }

    public void setSurfaceCount(final double surfaceCount) {
        this.surfaceCount = surfaceCount;
    }

    public int countNeighbors(int row, int col) {
        int sum = 0;

        if( isAlive(row-1,col) ) {
            sum++;
        }
        if( isAlive(row+1,col) ) {
            sum++;
        }
        if( isAlive(row,col-1) ) {
            sum++;
        }
        if( isAlive(row,col+1) ) {
            sum++;
        }

        if( isAlive(row-1,col-1) ) {
            sum++;
        }
        if( isAlive(row+1,col+1) ) {
            sum++;
        }
        if( isAlive(row-1,col+1) ) {
            sum++;
        }
        if( isAlive(row+1,col-1) ) {
            sum++;
        }

        return sum;
    }

    public boolean canGrow(int row, int col) {
        PlantUniverseCell cell = getCell(row,col);
        if( cell.isAlive() ) {
            if( row>=PlantUniverse.GROUND_LINE ) {
                return countNeighbors(row,col)<4;
            } else {
                return cell.getEnergy()>PlantUniverse.GROWTH_THRESHOLD && cell.getNourishment()>PlantUniverse.GROWTH_THRESHOLD;
            }
        }
        return false;
    }

    public boolean isAlive(final int row, final int col) {
        if( !isValid(row,col) ) {
            return false;
        }

        return(grid[row][col].isAlive());
    }
}
