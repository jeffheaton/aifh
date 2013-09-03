package com.heatonresearch.aifh.randomize;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 4:34 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractBoxMuller extends AbstractGenerateRandom {

    /**
     * The y2 value.
     */
    private double y2;

    /**
     * Should we use the last value.
     */
    private boolean useLast = false;

    /**
     * The mean.
     */
    public static final double MU = 0;

    /**
     * The standard deviation.
     */
    private static final double SIGMA = 1;


    @Override
    public double nextGaussian() {
        double x1, x2, w, y1;

        // use value from previous call
        if (this.useLast) {
            y1 = this.y2;
            this.useLast = false;
        } else {
            do {
                x1 = 2.0 * nextDouble() - 1.0;
                x2 = 2.0 * nextDouble() - 1.0;
                w = x1 * x1 + x2 * x2;
            } while (w >= 1.0);

            w = Math.sqrt((-2.0 * Math.log(w)) / w);
            y1 = x1 * w;
            this.y2 = x2 * w;
            this.useLast = true;
        }

        return (AbstractBoxMuller.MU + y1 * AbstractBoxMuller.SIGMA);
    }
}
