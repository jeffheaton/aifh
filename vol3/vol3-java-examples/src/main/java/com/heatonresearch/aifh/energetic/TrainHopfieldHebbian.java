package com.heatonresearch.aifh.energetic;

import com.heatonresearch.aifh.AIFHError;

/**
 * Train the Hopfield network using a Hebbian algorithm.
 * For more info: https://en.wikipedia.org/wiki/Hopfield_network
 */
public class TrainHopfieldHebbian {

    /**
     * The network to train.
     */
    private HopfieldNetwork network;

    /**
     * The summation matrix.
     */
    private double[][] sumMatrix;

    /**
     * The count of patterns.
     */
    private int patternCount;

    /**
     * Construct the trainer.
     * @param theNetwork The network to train.
     */
    public TrainHopfieldHebbian(HopfieldNetwork theNetwork) {
        this.network = theNetwork;
        this.sumMatrix = new double[network.getInputCount()][network.getInputCount()];
    }

    /**
     * Add a pattern to train.
     * @param pattern The pattern to train.
     */
    public void addPattern(double[] pattern) {
        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                if(i==j) {
                    this.sumMatrix[i][j] = 0;
                } else {
                    this.sumMatrix[i][j] += pattern[i] * pattern[j];
                }
            }
        }
        this.patternCount++;
    }

    /**
     * Learn the added patterns.
     */
    public void learn() {
        if( this.patternCount==0) {
            throw new AIFHError("Please add a pattern before learning.  Nothing to learn.");
        }

        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                this.network.setWeight(i,j,this.sumMatrix[i][j]/this.patternCount);
            }
        }
    }

}
