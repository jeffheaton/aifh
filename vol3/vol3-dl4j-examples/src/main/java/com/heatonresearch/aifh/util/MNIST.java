package com.heatonresearch.aifh.util;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.AIFHError;
import org.nd4j.linalg.api.ndarray.INDArray;

import java.io.File;

/**
 * Created by jeff on 1/19/16.
 */
public class MNIST {
    public static MNISTReader loadMNIST(String path, boolean training) {
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
                pathLabels.toString(),pathImages.toString());
    }

    public static void displayFirstMNIST(MNISTReader reader, int count) {
        for(int i=0;i<count;i++) {
            System.out.println("=========" + ArrayUtil.indexOfLargest(reader.getData().get(i).getLabels()));
            dumpMNISTDigit(reader.getData().get(i).getFeatures());
        }
    }

    public static void dumpMNISTDigit(INDArray data) {
        int idx = 0;
        for(int i=0;i<28;i++) {
            StringBuilder line = new StringBuilder();
            for(int j=0;j<28;j++) {
                line.append(data.getDouble(idx++)> AIFH.DEFAULT_PRECISION?"*":" ");
            }
            System.out.println(line.toString());
        }
    }
}
