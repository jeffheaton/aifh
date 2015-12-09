package com.heatonresearch.aifh.util;


import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.data.BasicData;

import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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
    private final List<BasicData> data;

    public MNISTReader(String labelFilename, String imageFilename, int depth) {
        try {
            if( depth!=1 && depth!=3 ) {
                throw new AIFHError("MNIST depth must be 1 or 3.");
            }


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
            if (numLabels != numImages) {
                StringBuilder str = new StringBuilder();
                str.append("Image file and label file do not contain the same number of entries.\n");
                str.append("  Label file contains: " + numLabels + "\n");
                str.append("  Image file contains: " + numImages + "\n");
                throw new AIFHError(str.toString());
            }

            byte[] labelsData = new byte[numLabels];
            labels.read(labelsData);
            int imageVectorSize = numCols * numRows;
            byte[] imagesData = new byte[numLabels * imageVectorSize];
            images.read(imagesData);

            this.data = new ArrayList<>();
            int imageIndex = 0;
            for(int i=0;i<this.numLabels;i++) {
                int label = labelsData[i];
                double[] inputData = new double[imageVectorSize*depth];
                int outputIndex = 0;
                for(int j=0;j<imageVectorSize;j++) {
                    int t = imageIndex++;
                    for(int k=0;k<depth;k++) {
                        inputData[outputIndex++] = ((double) (imagesData[t] & 0xff)) / 255.0;
                    }
                }
                double[] idealData = new double[10];
                idealData[label] = 1.0;
                this.data.add(new BasicData(inputData,idealData,null));
            }

            images.close();
            labels.close();

        } catch (IOException ex) {
            throw new AIFHError(ex);
        }
    }

    /**
     * @return the numLabels
     */
    public int getNumLabels() {
        return numLabels;
    }

    /**
     * @return the numImages
     */
    public int getNumImages() {
        return numImages;
    }

    /**
     * @return the numRows
     */
    public int getNumRows() {
        return numRows;
    }

    /**
     * @return the numCols
     */
    public int getNumCols() {
        return numCols;
    }

    /**
     * @return the data
     */
    public List<BasicData> getData() {
        return data;
    }



}
