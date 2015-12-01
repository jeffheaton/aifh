package com.heatonresearch.aifh.energetic;

/**
 * Train the Hopfield network using a Storkey algorithm.
 * For more info: https://en.wikipedia.org/wiki/Hopfield_network
 */
public class TrainHopfieldStorkey {
    /**
     * The network to train.
     */
    private HopfieldNetwork network;

    /**
     * The summation matrix.
     */
    private double[][] sumMatrix;

    public TrainHopfieldStorkey(HopfieldNetwork theNetwork) {
        this.network = theNetwork;
        this.sumMatrix = new double[network.getInputCount()][network.getInputCount()];
    }

    /**
     * Calculate the local field needed by training.
     * @param i The neuron.
     * @param pattern The pattern.
     * @return The local field value.
     */
    private double calculateLocalField(int i, double[] pattern) {
        double sum = 0;
        for(int k=0;k<this.network.getInputCount();k++) {
            if(k!=i) {
                sum += this.network.getWeight(i,k) * pattern[k];
            }
        }
        return sum;
    }

    /**
     * Add a pattern for training.
     * @param pattern The pattern to add.
     */
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
                double t2 = (pattern[i] * calculateLocalField(j,pattern))/n;
                double t3 = (pattern[j] * calculateLocalField(i,pattern))/n;
                double d = t1-t2-t3;
                this.sumMatrix[i][j]+=d;
            }
        }

        for(int i=0;i<sumMatrix.length;i++) {
            for(int j=0;j<sumMatrix.length;j++) {
                network.setWeight(i,j,network.getWeight(i,j)+this.sumMatrix[i][j]);
            }
        }
    }


}
