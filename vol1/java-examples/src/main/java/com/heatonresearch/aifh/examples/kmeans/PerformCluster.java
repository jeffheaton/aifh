package com.heatonresearch.aifh.examples.kmeans;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.kmeans.Cluster;
import com.heatonresearch.aifh.kmeans.KMeans;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.File;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 9:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class PerformCluster {
    public static void main(String[] args) {
        if (args.length > 2 || args.length < 1) {
            System.out.println("Usage:\nPerformCluster [input file] [optional label field #]");
        }


        DataSet ds = DataSet.load(new File("/Users/jheaton/temp/iris.csv"));
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
    }
}
