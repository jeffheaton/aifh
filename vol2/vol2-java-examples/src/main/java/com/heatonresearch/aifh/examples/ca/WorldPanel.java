package com.heatonresearch.aifh.examples.ca;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/19/14
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class WorldPanel extends JPanel {

    private boolean[][] primaryGrid;
    private boolean[][] backupGrid;
    private boolean showGrid;

    public WorldPanel(final int rows, final int cols, boolean showGrid) {
        this.primaryGrid = new boolean[rows][cols];
        this.backupGrid = new boolean[rows][cols];
        this.showGrid = showGrid;
    }

    public int getRows() {
        return this.primaryGrid.length;
    }

    public int getCols() {
        return this.primaryGrid[0].length;
    }

    public boolean[][] getPrimaryGrid() {
        return this.primaryGrid;
    }

    public boolean[][] getBackupGrid() {
        return this.backupGrid;
    }


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

    public void advanceBackupGrid() {
        for(int row=0;row<getRows();row++) {
            for(int col=0;col<getCols();col++) {
                this.primaryGrid[row][col] = this.backupGrid[row][col];
            }
        }
    }



}
