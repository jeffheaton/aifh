package com.heatonresearch.aifh.examples.model;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.examples.ga.TSPScore;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.genetic.crossover.Splice;
import com.heatonresearch.aifh.genetic.crossover.SpliceNoRepeat;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenome;
import com.heatonresearch.aifh.genetic.genome.IntegerArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.score.CalculateScore;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/20/14
 * Time: 8:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class ModelIris extends SimpleLearn {

    /**
     * The size of the population.
     */
    public static final int POPULATION_SIZE = 1000;


    public static Population initPopulation(GenerateRandom rnd, int inputCount, int rbfCount, int outputCount) {
        final RBFNetwork network = new RBFNetwork(inputCount, rbfCount, outputCount);
        int size = network.getLongTermMemory().length;

        Population result = new BasicPopulation(POPULATION_SIZE, null);

        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        for (int i = 0; i < POPULATION_SIZE; i++) {
            final DoubleArrayGenome genome = new DoubleArrayGenome(size);
            network.reset(rnd);
            System.arraycopy(network.getLongTermMemory(),0,genome.getData(),0,size);
            defaultSpecies.getMembers().add(genome);
        }
        result.setGenomeFactory(new DoubleArrayGenomeFactory(size));
        result.getSpecies().add(defaultSpecies);

        return result;

    }


    /**
     * Run the example.
     */
    public void process() {
        try {
            final InputStream istream = this.getClass().getResourceAsStream("/iris.csv");
            if( istream==null ) {
                System.out.println("Cannot access data set, make sure the resources are available.");
                System.exit(1);
            }
            GenerateRandom rnd = new MersenneTwisterGenerateRandom();

            final DataSet ds = DataSet.load(istream);
            // The following ranges are setup for the Iris data set.  If you wish to normalize other files you will
            // need to modify the below function calls other files.
            ds.normalizeRange(0, 0, 1);
            ds.normalizeRange(1, 0, 1);
            ds.normalizeRange(2, 0, 1);
            ds.normalizeRange(3, 0, 1);
            final Map<String, Integer> species = ds.encodeEquilateral(4);
            istream.close();

            final List<BasicData> trainingData = ds.extractSupervised(0, 4, 4, 2);

            final RBFNetwork network = new RBFNetwork(4, 4, 2);

            Population pop = initPopulation(rnd,4,4,2);

            //CalculateScore score = new ScoreRegressionData(trainingData);

            //BasicEA genetic = new BasicEA(pop,score);

            //genetic.addOperation(0.9,new Splice(network.getLongTermMemory().length/3));
            //genetic.addOperation(0.1,new MutateShuffle());


//            performIterations(genetic, 100000, 0.01, true);
//            queryEquilateral(network, trainingData, species, 0, 1);


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }

    public static void main(final String[] args) {
        final ModelIris prg = new ModelIris();
        prg.process();
    }
}
