package com.heatonresearch.aifh.examples.modeling;

import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.activation.ActivationSoftMax;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.DataUtil;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class KaggleOtto extends SimpleLearn {
    public static File KAGGLE_PATH = new File("/Users/jeff/data/kaggle/otto");
    public static File KAGGLE_TRAIN = new File(KAGGLE_PATH,"train.csv");
    public static File KAGGLE_TEST = new File(KAGGLE_PATH,"test.csv");
    public static File KAGGLE_SUBMIT = new File(KAGGLE_PATH,"submit.csv");
    public static File KAGGLE_DUMP = new File(KAGGLE_PATH,"dump.csv");

    public void createSubmission(BasicNetwork network) throws IOException {
        System.out.println("Building submission file.");
        FileInputStream istream = new FileInputStream(KAGGLE_TEST);
        final DataSet ds = DataSet.load(istream);
        istream.close();
        int columnCount = ds.getHeaderCount();

        List<String> ids = ds.columnAsList(0);
        ds.deleteColumn(0);

        for(int i=0;i<columnCount-1;i++) {
            ds.normalizeZScore(i);
        }

        final List<BasicData> data = ds.extractSupervised(0, columnCount-1, 0, 0);

        CSVWriter writer = new CSVWriter(new FileWriter(KAGGLE_SUBMIT));
        for(int i = 0; i<data.size(); i++) {
            double[] output = network.computeRegression(data.get(i).getInput());
            String[] line = new String[10];
            line[0] = ids.get(i);
            for(int j=0;j<output.length;j++) {
                line[j+1] = String.format(Locale.ENGLISH, "%.2f", output[j]);
            }
            writer.writeNext(line);

        }
        writer.close();
    }


    public void process() throws IOException {
        if( !KAGGLE_TRAIN.exists() || !KAGGLE_TEST.exists() ) {
            System.out.println("Can't find Kaggle datafiles.  Please modify KAGGLE_PATH to point to a folder " +
                    "containing train.csv and test.csv.  These files can be downloaded " +
                    "from: https://www.kaggle.com/c/otto-group-product-classification-challenge");
            System.exit(1);
        }

        FileInputStream istream = new FileInputStream(KAGGLE_TRAIN);
        final DataSet ds = DataSet.load(istream);
        istream.close();
        int columnCount = ds.getHeaderCount();
        System.out.println("Columns: " + columnCount);
        System.out.println("Rows: " + ds.size());

        ds.deleteColumn(0); // id
        columnCount--;

        for(int i=0;i<columnCount-1;i++) {
            //ds.normalizeZScore(i);
        }
        Map<String, Integer> classes = ds.encodeOneOfN(columnCount-1);

        BasicNetwork network = new BasicNetwork();
        network.addLayer(new BasicLayer(null,true,columnCount-1));
        //network.addLayer(new BasicLayer(new ActivationReLU(),true,300));
        //network.addLayer(new BasicLayer(new ActivationReLU(),true,200));
        network.addLayer(new BasicLayer(new ActivationReLU(),true,500));
        network.addLayer(new BasicLayer(new ActivationSoftMax(),false,classes.size()));
        network.finalizeStructure();
        network.reset();

        final List<BasicData> trainingData = ds.extractSupervised(0, columnCount-1, columnCount-1, classes.size());

        //DataUtil.dumpCSV(KAGGLE_DUMP,trainingData);

        final BackPropagation train = new BackPropagation(network, trainingData, 1e-5, 0.0);
        train.setBatchSize(0);
        performIterations(train, 10000, 0.01, true);

        // generate a submission file
        createSubmission(network);
    }

    public static void main(String[] args) {
        try {
            KaggleOtto prg = new KaggleOtto();
            prg.process();
        } catch(Throwable t) {
            t.printStackTrace();
        }
    }
}
