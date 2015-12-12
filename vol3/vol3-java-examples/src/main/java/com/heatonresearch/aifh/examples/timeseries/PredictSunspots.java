package com.heatonresearch.aifh.examples.timeseries;

import au.com.bytecode.opencsv.CSVReader;
import com.heatonresearch.aifh.ann.BasicLayer;
import com.heatonresearch.aifh.ann.BasicNetwork;
import com.heatonresearch.aifh.ann.activation.ActivationLinear;
import com.heatonresearch.aifh.ann.activation.ActivationReLU;
import com.heatonresearch.aifh.ann.train.BackPropagation;
import com.heatonresearch.aifh.examples.learning.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.general.data.TimeSeriesUtil;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;

public class PredictSunspots extends SimpleLearn {

    public int INPUT_WINDOW = 10;

    public List<BasicData> loadSunspots() throws IOException {
        // Obtain file
        final InputStream istream = this.getClass().getResourceAsStream("/sunspots.csv");
        if( istream==null ) {
            System.out.println("Cannot access data set, make sure the resources are available.");
            System.exit(1);
        }

        final Reader reader = new InputStreamReader(istream);
        final CSVReader csv = new CSVReader(reader);
        csv.readNext();

        // determine how many entries in file
        int sunspotCount = 0;
        while ( csv.readNext() != null) {
            sunspotCount++;
        }
        System.out.println("Sunspot count:" + sunspotCount);

        // allocate array to hold file
        double[][] dataset = new double[sunspotCount][1];

        // read file
        final InputStream istream2 = this.getClass().getResourceAsStream("/sunspots.csv");
        final Reader reader2 = new InputStreamReader(istream2);
        final CSVReader csv2 = new CSVReader(reader2);
        csv2.readNext();

        String[] nextLine;
        int idx = 0;
        while ((nextLine = csv2.readNext()) != null) {
            double ssn = Double.parseDouble(nextLine[3]);
            dataset[idx++][0] = ssn;
        }

        // timseries encode
        List<BasicData> result = TimeSeriesUtil.slidingWindow(dataset,INPUT_WINDOW,1,new int[]{0},new int[]{0});

        return result;
    }

    public void process() {
        try {
            List<BasicData> trainingData = loadSunspots();

            BasicNetwork network = new BasicNetwork();
            network.addLayer(new BasicLayer(null,true,INPUT_WINDOW));
            network.addLayer(new BasicLayer(new ActivationReLU(),true,50));
            network.addLayer(new BasicLayer(new ActivationLinear(),false,1));
            network.finalizeStructure();
            network.reset();

            final BackPropagation train = new BackPropagation(network, trainingData, 1e-9, 0.5);
            train.setBatchSize(0);

            performIterations(train, 100000, 650, true);
            query(network,trainingData);


        } catch(Throwable t) {
            t.printStackTrace();
        }
    }

    public static void main(String[] args) {
        PredictSunspots prg = new PredictSunspots();
        prg.process();
    }

}
