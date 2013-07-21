package com.heatonresearch.aifh.randomize.example;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/21/13
 * Time: 2:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class HistoPanel extends JPanel {

    public final int BAR_WIDTH = 10;

    private long evalCount;
    private long evalTime;
    private int evalRate;
    private long lastUpdate = -1;
    private long generated = 0;
    private long started;
    private int runtime;
    private double low = Double.POSITIVE_INFINITY;
    private double high = Double.NEGATIVE_INFINITY;
    private int[] boxes;

    public HistoPanel() {
        this.boxes = new int[2000];

    }

    public void reset() {
        this.started = -1;
        this.low = Double.POSITIVE_INFINITY;
        this.high = Double.NEGATIVE_INFINITY;
        this.runtime = 0;
    }

    public void reportNumber(double d) {
        if (lastUpdate == -1) {
            lastUpdate = System.currentTimeMillis();
            started = lastUpdate;
        }

        long currentTime = System.currentTimeMillis();
        generated++;
        this.low = Math.min(this.low, d);
        this.high = Math.max(this.high, d);

        int boxNumber = (int) (d * 200) + 1000;
        if (boxNumber >= 0 && boxNumber < this.boxes.length) {
            this.boxes[boxNumber]++;
        }


        this.evalTime = (currentTime - started) - 5000;

        if (evalTime > 0) {
            evalCount++;
        }


        if ((currentTime - lastUpdate) > 1000) {
            this.runtime = (int) ((currentTime - started) / 1000);
            lastUpdate = currentTime;
            if (evalCount > 0) {
                evalRate = (int) (evalCount / evalTime);
            } else {
                evalRate = 0;
            }
            repaint();
        }
    }

    public void paint(Graphics g) {
        int height = this.getHeight();
        int width = this.getWidth();

        g.clearRect(0, 0, width, height);
        long gen = generated;
        gen /= 1024;

        int h = g.getFontMetrics().getHeight();

        int y = h;
        g.drawString("Running for: " + runtime + " seconds", 0, y);
        y += h;
        g.drawString("Generated: " + gen + " thousand numbers", 0, y);
        y += h;
        g.drawString("Range: " + this.low + " to " + this.high, 0, y);
        y += h;
        g.drawString("Rate: " + this.evalRate + " #s/ms", 0, y);

        int barCount = width / BAR_WIDTH;
        int mode = 0;
        for (int i = 0; i < this.boxes.length; i++) {
            mode = Math.max(mode, boxes[i]);
        }
        int bar2box = this.boxes.length / barCount;
        int boxesIndex = 0;

        for (int i = 0; i < barCount; i++) {
            int barAmount = 0;
            for (int j = 0; j < bar2box; j++) {
                barAmount += this.boxes[boxesIndex++];
            }
            barAmount /= bar2box;


            double barRatio = (double) barAmount / mode;
            int barHeight = (int) (barRatio * height);

            g.setColor(Color.CYAN);
            g.fillRect(i * BAR_WIDTH, height - barHeight, BAR_WIDTH, barHeight);
            g.setColor(Color.BLACK);
            g.drawRect(i * BAR_WIDTH, height - barHeight, BAR_WIDTH, barHeight);
        }


    }
}
