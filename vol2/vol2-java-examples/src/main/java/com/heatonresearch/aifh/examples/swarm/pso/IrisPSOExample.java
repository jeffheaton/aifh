package com.heatonresearch.aifh.examples.swarm.pso;

import com.heatonresearch.aifh.examples.util.SimpleLearn;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainPSO;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * Learn the Iris data set with a RBF network trained by PSO.
 */
public class IrisPSOExample extends SimpleLearn {

    /**
     * The number of particles
     */
    public static final int PARTICLE_COUNT = 30;

    public static void main(final String[] args) {
        final IrisPSOExample prg = new IrisPSOExample();
        prg.process();
    }

    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if (istream == null) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }

            GenerateRandom rnd = new MersenneTwisterGenerateRandom();

            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, -1, 1);
            ds.normalizeRange(1, -1, 1);
            ds.normalizeRange(2, -1, 1);
            ds.normalizeRange(3, -1, 1);
            final Map<String, Integer> species = ds.encodeOneOfN(4);
            istream.close();

            RBFNetwork[] particles = new RBFNetwork[PARTICLE_COUNT];
            for(int i=0;i<particles.length;i++) {
                particles[i] = new RBFNetwork(4,4,3);
                particles[i].reset(rnd);
            }

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 3);

            ScoreFunction score = new ScoreRegressionData(trainingData);

            TrainPSO train = new TrainPSO(particles,score);

            performIterations(train, 100000, 0.05, true);

            RBFNetwork winner = (RBFNetwork)train.getBestParticle();

            queryOneOfN(winner, trainingData, species);


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }
}
