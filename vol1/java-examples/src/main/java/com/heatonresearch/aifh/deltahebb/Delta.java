package com.heatonresearch.aifh.deltahebb;

/**
 * Chapter 4: Machine Learning
 * <p/>
 * Delta: Learn, using the delta rule.
 *
 * @author Jeff Heaton
 * @version 2.1
 */
public class Delta {

    /**
     * Main method just instanciates a delta object and calls run.
     *
     * @param args Not used
     */
    public static void main(final String args[]) {
        final Delta delta = new Delta();
        delta.run();

    }

    /**
     * Weight for neuron 1
     */
    double w1;

    /**
     * Weight for neuron 2
     */
    double w2;

    /**
     * Weight for neuron 3
     */
    double w3;

    /**
     * Learning rate
     */
    double rate = 0.5;

    /**
     * Current epoch #
     */
    int epoch = 1;

    /**
     * Process one epoch. Here we learn from all three training samples and then
     * update the weights based on error.
     */

    protected void epoch() {
        System.out.println("***Beginning Epoch #" + this.epoch + "***");
        presentPattern(0, 0, 1, 0);
        presentPattern(0, 1, 1, 0);
        presentPattern(1, 0, 1, 0);
        presentPattern(1, 1, 1, 1);
        this.epoch++;
    }

    /**
     * This method will calculate the error between the anticipated output and
     * the actual output.
     *
     * @param actual      The actual output from the neural network.
     * @param anticipated The anticipated neuron output.
     * @return The error.
     */
    protected double getError(final double actual, final double anticipated) {
        return (anticipated - actual);
    }

    /**
     * Present a pattern and learn from it.
     *
     * @param i1          Input to neuron 1
     * @param i2          Input to neuron 2
     * @param i3          Input to neuron 3
     * @param anticipated The anticipated output
     */
    protected void presentPattern(final double i1, final double i2,
                                  final double i3, final double anticipated) {
        double error;
        double actual;
        double delta;

        // run the net as is on training data
        // and get the error
        System.out.print("Presented [" + i1 + "," + i2 + "," + i3 + "]");
        actual = recognize(i1, i2, i3);
        error = getError(actual, anticipated);
        System.out.print(" anticipated=" + anticipated);
        System.out.print(" actual=" + actual);
        System.out.println(" error=" + error);

        // adjust weight 1
        delta = trainingFunction(this.rate, i1, error);
        this.w1 += delta;

        // adjust weight 2
        delta = trainingFunction(this.rate, i2, error);
        this.w2 += delta;

        // adjust weight 3
        delta = trainingFunction(this.rate, i3, error);
        this.w3 += delta;
    }

    /**
     * @param i1 Input to neuron 1
     * @param i2 Input to neuron 2
     * @param i3 Input to neuron 3
     * @return the output from the neural network
     */
    protected double recognize(final double i1, final double i2, final double i3) {
        final double a = (this.w1 * i1) + (this.w2 * i2) + (this.w3 * i3);
        return (a * .5);
    }

    /**
     * This method loops through 100 epochs.
     */
    public void run() {
        for (int i = 0; i < 100; i++) {
            epoch();
        }
    }

    /**
     * The learningFunction implements the delta rule. This method will return
     * the weight adjustment for the specified input neuron.
     *
     * @param rate  The learning rate
     * @param input The input neuron we're processing
     * @param error The error between the actual output and anticipated output.
     * @return The amount to adjust the weight by.
     */
    protected double trainingFunction(final double rate, final double input,
                                      final double error) {
        return rate * input * error;
    }
}