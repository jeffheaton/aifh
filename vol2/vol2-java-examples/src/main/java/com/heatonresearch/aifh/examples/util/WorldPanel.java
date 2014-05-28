package com.heatonresearch.aifh.examples.util;

import javax.swing.*;
import java.awt.*;

/**
 * This class is used to display a grid.  This grid is often used to simulate cellular automation.
 */
public class WorldPanel extends JPanel {

    /**
     * The primary, displayed grid.
     */
    private boolean[][] primaryGrid;

    /**
     * The backup grid.
     */
    private boolean[][] backupGrid;

    /**
     * The grid currently being shown.
     */
    private boolean showGrid;

    /**
     * The world panel constructor.
     * @param rows The number of rows in the grid.
     * @param cols The number of columns in the grid.
     * @param showGrid True, if the grid is to be shown.
     */
    public WorldPanel(final int rows, final int cols, boolean showGrid) {
        this.primaryGrid = new boolean[rows][cols];
        this.backupGrid = new boolean[rows][cols];
        this.showGrid = showGrid;
    }

    /**
     * @return The number of rows.
     */
    public int getRows() {
        return this.primaryGrid.length;
    }

    /**
     * @return The number of columns.
     */
    public int getCols() {
        return this.primaryGrid[0].length;
    }

    /**
     * @return The primary grid.
     */
    public boolean[][] getPrimaryGrid() {
        return this.primaryGrid;
    }

    /**
     * @return The backup grid.
     */
    public boolean[][] getBackupGrid() {
        return this.backupGrid;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void paint(Graphics g)
    {
        super.paint(g);

        int width = this.getWidth();
        int height = this.getHeight();

        double cellWidth = ((double)width/(double)getCols());
        double cellHeight = ((double)height/(double)getRows());

        g.setColor(Color.WHITE);
        g.fillRect(0,0,width,height);

        if( this.showGrid ) {
            g.setColor(Color.black);
            for(int row=0;row<getRows();row++) {
                int y = (int)(row*cellHeight);
                g.drawLine(0,y,width,y);
            }

            for(int col=0;col<getCols();col++) {
                int x = (int)(col*cellWidth);
                g.drawLine(x,0,x,height);
            }
        }

        for(int row=0;row<getRows();row++) {
            for(int col=0;col<getCols();col++) {
                int x = (int)(col*cellWidth);
                int y = (int)(row*cellHeight);

                if( this.primaryGrid[row][col] ) {
                    g.setColor(Color.black);
                    g.fillRect(x,y,(int)cellWidth,(int)cellHeight);
                }
            }
        }
    }

    /**
     * Advance backup grid to primary.
     */
    public void advanceBackupGrid() {
        for(int row=0;row<getRows();row++) {
            for(int col=0;col<getCols();col++) {
                this.primaryGrid[row][col] = this.backupGrid[row][col];
            }
        }
    }



}
