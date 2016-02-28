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
package com.heatonresearch.aifh.util;


import com.heatonresearch.aifh.AIFHError;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.DataSet;
import org.nd4j.linalg.factory.Nd4j;

import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.zip.GZIPInputStream;

/**
 * This reads the MNIST dataset of handwritten digits into a data set.
 * The MNIST dataset is found at http://yann.lecun.com/exdb/mnist/.
 *
 * Very loosely adapted from a class by Gabe Johnson <johnsogg@cmu.edu>.
 * https://code.google.com
 * /p/pen-ui/source/browse/trunk/skrui/src/org/six11/skrui
 * /charrec/MNISTReader.java?r=185
 */
public class MNISTReader {

    private final int numLabels;
    private final int numImages;
    private final int numRows;
    private final int numCols;
    private final DataSet data;

    public MNISTReader(String labelFilename, String imageFilename) {
        try {
            DataInputStream labels;
            DataInputStream images;

            // Read label file, decompress (as read in) if needed.
            if( labelFilename.toLowerCase().endsWith(".gz")) {
                labels = new DataInputStream(new GZIPInputStream(new FileInputStream(
                        labelFilename)));
            } else {
                labels = new DataInputStream(new FileInputStream(
                        labelFilename));
            }

            // Read images file, decompress (as read in) if needed.
            if( imageFilename.toLowerCase().endsWith(".gz")) {
                images = new DataInputStream(new GZIPInputStream(new FileInputStream(
                        imageFilename)));
            } else {
                images = new DataInputStream(new FileInputStream(
                        imageFilename));
            }


            int magicNumber = labels.readInt();
            if (magicNumber != 2049) {
                throw new AIFHError("Label file has wrong magic number: "
                        + magicNumber + " (should be 2049)");
            }
            magicNumber = images.readInt();
            if (magicNumber != 2051) {
                throw new AIFHError("Image file has wrong magic number: "
                        + magicNumber + " (should be 2051)");
            }
            this.numLabels = labels.readInt();
            this.numImages = images.readInt();
            this.numRows = images.readInt();
            this.numCols = images.readInt();
            if (this.numLabels != this.numImages) {
                StringBuilder str = new StringBuilder();
                str.append("Image file and label file do not contain the same number of entries.\n");
                str.append("  Label file contains: " + this.numLabels + "\n");
                str.append("  Image file contains: " + this.numImages + "\n");
                throw new AIFHError(str.toString());
            }

            byte[] labelsData = new byte[this.numLabels];
            labels.readFully(labelsData);
            int imageVectorSize = this.numCols * this.numRows;
            byte[] imagesData = new byte[this.numLabels * imageVectorSize];
            images.readFully(imagesData);

            INDArray training = Nd4j.create(this.numLabels,imageVectorSize);
            INDArray ideal = Nd4j.create(this.numLabels,10);

            int imageIndex = 0;
            for(int i=0;i<this.numLabels;i++) {
                int label = labelsData[i];
                for (int j = 0; j < imageVectorSize; j++) {
                    training.put(i,j, ((double) (imagesData[imageIndex++] & 0xff)) / 255.0);
                }
                for(int j=0;j<10;j++) {
                    ideal.put(i, j, j==label?1:0);
                }
            }

            images.close();
            labels.close();

            this.data = new DataSet(training,ideal);

        } catch (IOException ex) {
            throw new AIFHError(ex);
        }
    }

    /**
     * @return the numLabels
     */
    public int getNumLabels() {
        return this.numLabels;
    }

    /**
     * @return the numImages
     */
    public int getNumImages() {
        return this.numImages;
    }

    /**
     * @return the numRows
     */
    public int getNumRows() {
        return this.numRows;
    }

    /**
     * @return the numCols
     */
    public int getNumCols() {
        return this.numCols;
    }

    /**
     * @return the data
     */
    public DataSet getData() {
        return this.data;
    }



}
