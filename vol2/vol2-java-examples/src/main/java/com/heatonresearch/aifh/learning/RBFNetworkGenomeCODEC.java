package com.heatonresearch.aifh.learning;

import com.heatonresearch.aifh.evolutionary.codec.GeneticCODEC;
import com.heatonresearch.aifh.evolutionary.genome.Genome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/21/14
 * Time: 5:52 AM
 * To change this template use File | Settings | File Templates.
 */
public class RBFNetworkGenomeCODEC implements GeneticCODEC {

    public int getInputCount() {
        return inputCount;
    }

    private int inputCount;

    public int getOutputCount() {
        return outputCount;
    }

    public int getRbfCount() {
        return rbfCount;
    }

    public int size() {
        return size;
    }

    private int outputCount;
    private int rbfCount;
    private int size;

    public RBFNetworkGenomeCODEC(int inputCount,int rbfCount, int outputCount) {
        this.inputCount = inputCount;
        this.rbfCount = rbfCount;
        this.outputCount = outputCount;
        RBFNetwork temp = new RBFNetwork(inputCount,rbfCount,outputCount);
        this.size = temp.getLongTermMemory().length;
    }

    @Override
    public MLMethod decode(final Genome genome) {
        RBFNetwork result = new RBFNetwork(inputCount,rbfCount,outputCount);
        DoubleArrayGenome dag = (DoubleArrayGenome)genome;
        System.arraycopy(dag.getData(),0,result.getLongTermMemory(),0,size);
        return result;
    }

    @Override
    public Genome encode(final MLMethod phenotype) {
        RBFNetwork rbfNet = (RBFNetwork)phenotype;
        DoubleArrayGenome result = new DoubleArrayGenome(size());
        System.arraycopy(rbfNet.getLongTermMemory(),0,result.getData(),0,size);
        return result;
    }
}
