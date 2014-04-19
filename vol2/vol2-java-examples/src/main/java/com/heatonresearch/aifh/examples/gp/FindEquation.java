package com.heatonresearch.aifh.examples.gp;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.normalize.DataSet;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/18/14
 * Time: 9:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class FindEquation {

    public static void main(String[] args) {
        FindEquation prg = new FindEquation();
        if( args.length==0) {
            prg.process(null);
        } else if( args.length==1) {
            prg.process(args[0]);
        } else {
            System.out.println("Specify a filename to fit, or no filename to use a built in simple polynomial.");
            System.exit(1);
        }

    }

    public void process(final String filename) {
        InputStream istream = null;

        // If no file is provided, try to use the simple polynomial data from the resources.
        if (filename == null) {
            istream = this.getClass().getResourceAsStream("/simple-poly.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
        } else {
            // If a file is provided, try to read from that file.
            try {
                istream = new FileInputStream(filename);
            } catch (IOException ex) {
                ex.printStackTrace();
                System.exit(1);
            }
        }

        // Load the file and obtain training data.
        final DataSet ds = DataSet.load(istream);
        // Extract supervised training.
        List<BasicData> training = ds.extractSupervised(0, 1, 1, 1);


    }
}
