package com.heatonresearch.aifh.examples.capstone.alife.milestone1;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/20/14
 * Time: 6:00 AM
 * To change this template use File | Settings | File Templates.
 */
public class PlantUniverse {
    public static final int UNIVERSE_WIDTH = 50;
    public static final int UNIVERSE_HEIGHT = 100;
    public static final int GROUND_LINE = UNIVERSE_HEIGHT-(UNIVERSE_HEIGHT /3);
    public static final int CELL_VECTOR_LENGTH = 4;
    public static final int GENOME_SIZE = CELL_VECTOR_LENGTH * 4;

    public static final double SUNLIGHT_DECAY = 0.1;
    public static final double NOURISHMENT_DECAY = 0.1;
    public static final double STEM_TRANSITION = 0.8;
    public static final double GROWTH_THRESHOLD = 0.25;
    public static final double MIN_LIVING_ENERGY = 0.1;
    public static final double REQUIRED_ROOT_RATIO = 0.0;

    public static final int POPULATION_SIZE = 1000;
    public static final int MAX_SAME_SOLUTION = 100;


    private PlantUniverseCell[][] grid = new PlantUniverseCell[UNIVERSE_HEIGHT][UNIVERSE_WIDTH];
    private int rootCount;
    private int surfaceCount;

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
        result[1] = cell.getCalculatedSunlight();
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
        grid[groundLevel][center].setComposition(1);
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

    public double calculateMaxNeighborsNourishment(final int row, final int col) {
        double result = 0;
        result=Math.max(result, calculateNourishment(row - 1, col - 1));
        result=Math.max(result, calculateNourishment(row - 1, col));
        result=Math.max(result, calculateNourishment(row - 1, col + 1));

        result=Math.max(result, calculateNourishment(row, col - 1));
        result=Math.max(result, calculateNourishment(row, col + 1));

        result=Math.max(result, calculateNourishment(row + 1, col - 1));
        result=Math.max(result, calculateNourishment(row + 1, col));
        result=Math.max(result, calculateNourishment(row + 1, col + 1));

        return result;
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

    public double calculateWater(final int row, final int col) {
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

    public int getRootCount() {
        return rootCount;
    }

    public void setRootCount(final int rootCount) {
        this.rootCount = rootCount;
    }

    public int getSurfaceCount() {
        return surfaceCount;
    }

    public void setSurfaceCount(final int surfaceCount) {
        this.surfaceCount = surfaceCount;
    }
}
