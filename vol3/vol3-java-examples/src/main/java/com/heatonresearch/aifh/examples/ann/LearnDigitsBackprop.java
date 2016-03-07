/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.ann;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.DataUtil;
import com.heatonresearch.aifh.util.ArrayUtil;
import com.heatonresearch.aifh.util.FileUtil;
import com.heatonresearch.aifh.util.MNISTReader;

import java.io.File;

/**
 * http://yann.lecun.com/exdb/mnist/
 */
public class LearnDigitsBackprop extends SimpleLearn {

    public static final int MNIST_DEPTH = 1;

    public static void dump(double[] data) {
        int idx = 0;
        for(int i=0;i<28;i++) {
            StringBuilder line = new StringBuilder();
            for(int j=0;j<28;j++) {
                line.append(data[idx++]> AIFH.DEFAULT_PRECISION?"*":" ");
            }
        }
    }

    public static MNISTReader loadMNIST(String path, boolean training, int depth) {
        File path2 = new File(path);
        String imagesFilename;
        String labelsFilename;

        if ( training ) {
            imagesFilename = "train-images-idx3-ubyte";
            labelsFilename = "train-labels-idx1-ubyte";
        } else {
            imagesFilename = "t10k-images-idx3-ubyte";
            labelsFilename = "t10k-labels-idx1-ubyte";
        }

        File pathImages = new File(path2,imagesFilename);
        File pathLabels = new File(path2,labelsFilename);

        if( !pathImages.exists() ) {
            imagesFilename += ".gz";
            pathImages = new File(path2,imagesFilename);
        }

        if( !pathLabels.exists() ) {
            labelsFilename += ".gz";
            pathLabels = new File(path2,labelsFilename);
        }

        if( !pathImages.exists() ) {
            // download
            System.out.println("Please wait, downloading digits from: http://yann.lecun.com");
            FileUtil.downloadFile("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
                    new File(path,"train-images-idx3-ubyte.gz"));
            FileUtil.downloadFile("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
                    new File(path,"train-labels-idx1-ubyte.gz"));
            FileUtil.downloadFile("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
                    new File(path,"t10k-images-idx3-ubyte.gz"));
            FileUtil.downloadFile("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
                    new File(path,"t10k-labels-idx1-ubyte.gz"));

        }

        if( !pathImages.exists() ) {
            throw( new AIFHError("Can't open file (with or without .gz): "
                    + pathImages.toString() ));
        }

        if( !pathLabels.exists() ) {
            throw( new AIFHError("Can't open file (with or without .gz): "
                    + pathLabels.toString() ));
        }

        return new MNISTReader(
                pathLabels.toString(),pathImages.toString(),depth);
    }

    public void display(MNISTReader reader) {
        for(int i=0;i<10;i++) {
            System.out.println("=========" + ArrayUtil.indexOfLargest(reader.getData().get(i).getIdeal()));
            dump(reader.getData().get(i).getInput());
        }
    }

    public void process() {
        System.out.println("Please wait, reading MNIST training data.");
        String dir = System.getProperty("user.dir");
        MNISTReader trainingReader = loadMNIST(dir,true, MNIST_DEPTH);
        MNISTReader validationReader = loadMNIST(dir,false, MNIST_DEPTH);

        System.out.println("Training set size: " + trainingReader.getNumImages());
        System.out.println("Validation set size: " + validationReader.getNumImages());

        int inputCount = trainingReader.getData().get(0).getInput().length;
        int outputCount = trainingReader.getData().get(0).getIdeal().length;

        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,inputCount));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,100));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,50));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,25));
        network.addLayer(new BasicLayer(new ActivationSoftMax(),false,outputCount));
        network.finalizeStructure();
        network.reset();

        // train the neural network
        System.out.println("Training neural network.");
        final BackPropagation train = new BackPropagation(network, trainingReader.getData(), 1e-4, 0.9);
        train.setL1(0);
        train.setL2(1e-11);

        this.performIterationsClassifyEarlyStop(train, network, validationReader.getData(), 5);
        System.out.println("Final accuracy: Incorrect %"
                + DataUtil.calculateClassificationError(validationReader.getData(), network)*100);
    }

    public static void main(String[] args) {

        LearnDigitsBackprop prg = new LearnDigitsBackprop();
        prg.process();
    }
}
