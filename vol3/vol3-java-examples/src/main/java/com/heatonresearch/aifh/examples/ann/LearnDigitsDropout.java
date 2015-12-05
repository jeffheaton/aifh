package com.heatonresearch.aifh.examples.ann;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.DropoutLayer;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.util.ArrayUtil;
import com.heatonresearch.aifh.util.FileUtil;
import com.heatonresearch.aifh.util.MNISTReader;

import java.io.File;

/**
 * http://yann.lecun.com/exdb/mnist/
 */
public class LearnDigitsDropout extends SimpleLearn {




    public void process() {
        System.out.println("Please wait, reading MNIST training data.");
        String dir = System.getProperty("user.dir");
        MNISTReader trainingReader = LearnDigitsBackprop.loadMNIST(dir,true);
        MNISTReader validationReader = LearnDigitsBackprop.loadMNIST(dir,false);

        System.out.println("Training set size: " + trainingReader.getNumImages());
        System.out.println("Validation set size: " + validationReader.getNumImages());

        int inputCount = trainingReader.getData().get(0).getInput().length;
        int outputCount = trainingReader.getData().get(0).getIdeal().length;

        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,inputCount));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,50));
        network.addLayer(new DropoutLayer(new ActivationReLU(),true,25,0.5));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,5));
        network.addLayer(new BasicLayer(new ActivationSoftMax(),false,outputCount));
        network.finalizeStructure();
        network.reset();

        // train the neural network
        System.out.println("Training neural network.");
        final BackPropagation train = new BackPropagation(network, trainingReader.getData(), 1e-4, 0.9);
        train.setL1(0);
        train.setL2(1e-11);

        this.performIterationsClassifyEarlyStop(train, network, trainingReader.getData(), 5);
    }

    public static void main(String[] args) {

        LearnDigitsBackprop prg = new LearnDigitsBackprop();
        prg.process();
    }
}
