package com.heatonresearch.aifh.examples.ca;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/19/14
 * Time: 6:22 PM
 * To change this template use File | Settings | File Templates.
 */
public class ConwayCA extends JFrame implements ActionListener, WindowListener, Runnable {
    private JButton iterationButton;
    private JButton startButton;
    private JButton stopButton;
    private JButton resetButton;
    private Thread thread;
    private boolean requestStop;

    private JLabel status;
    private JScrollPane scroll;
    private WorldPanel worldArea;
    private static final int[] neighborsX = { 0,0,1,-1, -1, 1,-1, 1 };
    private static final int[] neighborsY = { 1,-1,0,0, -1,-1, 1, 1 };

    public static final int ROWS = 75;
    public static final int COLS = 75;

    public ConwayCA() {
        setSize(500, 500);
        setTitle("Conway's Game of Life");

        Container c = getContentPane();
        c.setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        c.add(buttonPanel, BorderLayout.NORTH);
        c.add(this.status=new JLabel(), BorderLayout.SOUTH);
        buttonPanel.add(iterationButton = new JButton("Iteration"));
        buttonPanel.add(startButton = new JButton("Start"));
        buttonPanel.add(stopButton = new JButton("Stop"));
        buttonPanel.add(resetButton = new JButton("Reset"));

        this.worldArea = new WorldPanel(ROWS,COLS,true);
        this.scroll = new JScrollPane(this.worldArea);
        c.add(this.scroll, BorderLayout.CENTER);
        iterationButton.addActionListener(this);
        startButton.addActionListener(this);
        stopButton.addActionListener(this);
        resetButton.addActionListener(this);

        this.addWindowListener(this);
        this.stopButton.setEnabled(false);

        performReset();
    }

    public void performIteration() {
        boolean[][] grid = this.worldArea.getPrimaryGrid();

        for(int row=0;row<grid.length;row++) {
            for(int col=0;col<grid[row].length;col++) {
                int total = 0;

                for(int i=0;i<neighborsX.length;i++) {
                    int nCol = col+neighborsX[i];
                    int nRow = row+neighborsY[i];
                    if( nCol>=0 && nCol<this.worldArea.getCols()) {
                        if( nRow>=0 && nRow<this.worldArea.getRows()) {
                            if( grid[nRow][nCol] ) {
                                total++;
                            }
                        }
                    }
                }


                boolean alive = grid[row][col];

                if( alive  ) {
                    // 1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
                    if( total<2 ) {
                        alive = false;
                    }
                    // 2. Any live cell with two or three live neighbors lives on to the next generation. (not needed)
                    // 3. Any live cell with more than three live neighbors dies, as if by overcrowding.
                    if( alive && total>3 ) {
                        alive = false;
                    }
                } else {
                    // 4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
                    if( total==3 ) {
                        alive = true;
                    }
                }

                this.worldArea.getBackupGrid()[row][col] = alive;
            }
        }

        this.worldArea.advanceBackupGrid();
    }

    public void performStart() {
        this.iterationButton.setEnabled(false);
        this.stopButton.setEnabled(true);
        this.startButton.setEnabled(false);
        this.thread = new Thread(this);
        this.thread.start();
    }

    public void performStop() {
        this.requestStop = true;
    }

    public void performReset() {
        boolean[][] grid = this.worldArea.getBackupGrid();
        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        for(int row = 0;row<this.worldArea.getRows();row++) {
            for(int col=0;col<this.worldArea.getCols();col++) {
                grid[row][col] = rnd.nextBoolean();
            }
        }

        this.worldArea.advanceBackupGrid();

    }

    @Override
    public void actionPerformed(ActionEvent ev) {
        if (ev.getSource() == iterationButton) {
            performIteration();
            repaint();
        } else if (ev.getSource() == startButton) {
            performStart();
        } else if (ev.getSource() == stopButton) {
            performStop();
        } else if (ev.getSource() == resetButton) {
            performReset();
        }

    }

    @Override
    public void windowActivated(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowClosed(WindowEvent arg0) {


    }

    @Override
    public void windowClosing(WindowEvent arg0) {

        System.exit(0);

    }

    @Override
    public void windowDeactivated(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowDeiconified(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowIconified(WindowEvent arg0) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowOpened(WindowEvent arg0) {
        performReset();
    }


    public static void main(String[] args) {
        try {
            JFrame f = new ConwayCA();
            f.setVisible(true);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    @Override
    public void run() {
        this.requestStop = false;
        while(!this.requestStop) {
            performIteration();
            this.repaint();
            try {
                Thread.sleep(100);
            } catch (InterruptedException ex) {
                ex.printStackTrace();
            }
        }

        // Update GUI
        this.iterationButton.setEnabled(true);
        this.stopButton.setEnabled(false);
        this.startButton.setEnabled(true);
    }
}
