package com.heatonresearch.aifh.energetic;

import com.heatonresearch.aifh.AIFHError;

/**
 * Created with IntelliJ IDEA.
 * User: Jeff
 * Date: 1/27/15
 * Time: 4:56 PM
 * To change this template use File | Settings | File Templates.
 */
public class TrainHopfieldHebbian {
    private HopfieldNetwork network;
    private double[][] sumMatrix;
    private int patternCount;

    public TrainHopfieldHebbian(HopfieldNetwork theNetwork) {
        if( theNetwork instanceof HopfieldNetwork ) {
            throw new AIFHError("This trainer cannot be used with a Hopfield-Tank network, only discrete Hopfield");
        }

        this.network = theNetwork;
        this.sumMatrix = new double[network.getInputCount()][network.getInputCount()];
    }

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
