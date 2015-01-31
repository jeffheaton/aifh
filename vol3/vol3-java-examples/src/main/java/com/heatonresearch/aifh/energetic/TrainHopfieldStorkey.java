package com.heatonresearch.aifh.energetic;

import com.heatonresearch.aifh.AIFHError;

/**
 * Created with IntelliJ IDEA.
 * User: Jeff
 * Date: 1/27/15
 * Time: 4:56 PM
 * To change this template use File | Settings | File Templates.
 */
public class TrainHopfieldStorkey {
    private HopfieldNetwork network;
    private double[][] sumMatrix;
    private int patternCount;

    public TrainHopfieldStorkey(HopfieldNetwork theNetwork) {
        this.network = theNetwork;
        this.sumMatrix = new double[network.getInputCount()][network.getInputCount()];
    }

    private double calculateLocalField(int i, int j, double[] pattern) {
        double sum = 0;
        for(int k=0;k<this.network.getInputCount();k++) {
            if(k!=i) {
                sum += this.network.getWeight(i,k) * pattern[k];
            }
        }
        return sum;
    }

    public void addPattern(double[] pattern) {
        for(int i=0;i<sumMatrix.length;i++) {
            for(int j=0;j<sumMatrix.length;j++) {
                this.sumMatrix[i][j] = 0;
            }
        }

        double n = this.network.getInputCount();
        for(int i=0;i<this.sumMatrix.length;i++) {
            for(int j=0;j<this.sumMatrix.length;j++) {
                double t1 = (pattern[i] * pattern[j])/n;
                double t2 = (pattern[i] * calculateLocalField(j,i,pattern))/n;
                double t3 = (pattern[j] * calculateLocalField(i,j,pattern))/n;
                double d = t1-t2-t3;
                this.sumMatrix[i][j]+=d;
            }
        }
        //this.patternCount++;

        for(int i=0;i<sumMatrix.length;i++) {
            for(int j=0;j<sumMatrix.length;j++) {
                network.setWeight(i,j,network.getWeight(i,j)+this.sumMatrix[i][j]);
            }
        }
    }


}
