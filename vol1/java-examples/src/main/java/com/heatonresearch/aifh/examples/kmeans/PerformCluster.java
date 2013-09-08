package com.heatonresearch.aifh.examples.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.kmeans.Cluster;
import com.heatonresearch.aifh.kmeans.KMeans;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.InputStream;
import java.util.List;

/**
 * Try to cluster the Iris data set.
 */
public class PerformCluster {

    /**
     * The main method.
     *
     * @param args Not used.
     */
    public static void main(String[] args) {
        PerformCluster prg = new PerformCluster();
        prg.run();
    }

    /**
     * Perform the example.
     */
    public void run() {
        try {
            InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            DataSet ds = DataSet.load(istream);
            istream.close();
            List<BasicData> observations = ds.extractUnsupervisedLabeled(4);
            KMeans kmeans = new KMeans(3);
            kmeans.initForgy(observations);
            int iterations = kmeans.iteration(1000);
            System.out.println("Finished after " + iterations + " iterations.");

            for (int i = 0; i < kmeans.getK(); i++) {
                Cluster cluster = kmeans.getClusters().get(i);
                System.out.println("* * * Cluster #" + i);
                for (BasicData d : cluster.getObservations()) {
                    System.out.println(d.toString());
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
