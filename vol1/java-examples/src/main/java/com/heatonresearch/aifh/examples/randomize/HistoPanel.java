package com.heatonresearch.aifh.examples.randomize;

import javax.swing.*;
import java.awt.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;

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
    private long lastUpdate;
    private long generated;
    private long started;
    private int runtime;
    private double low;
    private double high;
    private final int[] boxes;
    private boolean uniformMode;
    private final DecimalFormat formatter = new DecimalFormat("#.##");
    private final NumberFormat intFormatter = NumberFormat.getNumberInstance();

    public HistoPanel() {
        this.boxes = new int[2000];
        reset();
    }

    public void reset() {
        this.evalCount = 0;
        this.evalTime = 0;
        this.evalRate = 0;
        this.lastUpdate = -1;
        this.generated = 0;
        this.started = -1;
        this.low = Double.POSITIVE_INFINITY;
        this.high = Double.NEGATIVE_INFINITY;
        this.runtime = 0;
        for (int i = 0; i < boxes.length; i++) {
            this.boxes[i] = 0;
        }
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

        int boxNumber = (int) ((d * 300.0) + 1000.0);
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

    public boolean isUniformMode() {
        return uniformMode;
    }

    public void setUniformMode(final boolean uniformMode) {
        this.uniformMode = uniformMode;
    }

    public void paint(Graphics g) {
        int height = this.getHeight();
        int width = this.getWidth();

        g.clearRect(0, 0, width, height);
        long gen = generated;
        gen /= 1024;

        int h = g.getFontMetrics().getHeight();

        int barCount = width / BAR_WIDTH;
        int mode = 0;
        for (int i = 0; i < this.boxes.length; i++) {
            mode = Math.max(mode, boxes[i]);
        }

        int bar2box = 0;
        int boxesIndex = 0;

        if (this.uniformMode) {
            bar2box = 1;
            boxesIndex = (this.boxes.length / 2) + bar2box;
        } else {
            bar2box = this.boxes.length / barCount;
        }

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
            g.setColor(Color.LIGHT_GRAY);
            g.drawRect(i * BAR_WIDTH, height - barHeight, BAR_WIDTH, barHeight);
        }

        NumberFormat.getNumberInstance().format(35634646);
        int y = h;

        g.setColor(Color.BLACK);
        g.drawString("Running for: " + this.intFormatter.format(runtime) + " seconds", 0, y);
        y += h;
        g.drawString("Generated: " + this.intFormatter.format(gen) + " thousand numbers", 0, y);
        y += h;
        g.drawString("Range: " + this.formatter.format(this.low)
                + " to " + this.formatter.format(this.high), 0, y);
        y += h;
        g.drawString("Rate: " + this.intFormatter.format(this.evalRate) + " nums/ms", 0, y);


    }
}
