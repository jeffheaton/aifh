package com.heatonresearch.aifh.examples.model;

import com.heatonresearch.aifh.evolutionary.population.BasicPopulation;
import com.heatonresearch.aifh.evolutionary.population.Population;
import com.heatonresearch.aifh.evolutionary.species.BasicSpecies;
import com.heatonresearch.aifh.evolutionary.train.basic.BasicEA;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.genetic.crossover.Splice;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenome;
import com.heatonresearch.aifh.genetic.genome.DoubleArrayGenomeFactory;
import com.heatonresearch.aifh.genetic.mutate.MutatePerturb;
import com.heatonresearch.aifh.genetic.mutate.MutateShuffle;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.RBFNetworkGenomeCODEC;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.learning.score.ScoreRegressionData;
import com.heatonresearch.aifh.normalize.DataSet;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * Learn the Iris data set with a RBF network trained by a genetic algorithm.
 */
public class ModelIris extends SimpleLearn {

    /**
     * The size of the population.
     */
    public static final int POPULATION_SIZE = 1000;

    /**
     * Create an initial population.
     *
     * @param rnd   Random number generator.
     * @param codec The codec, the type of network to use.
     * @return The population.
     */
    public static Population initPopulation(GenerateRandom rnd, RBFNetworkGenomeCODEC codec) {
        // Create a RBF network to get the length.
        final RBFNetwork network = new RBFNetwork(codec.getInputCount(), codec.getRbfCount(), codec.getOutputCount());
        int size = network.getLongTermMemory().length;

        // Create a new population, use a single species.
        Population result = new BasicPopulation(POPULATION_SIZE, new DoubleArrayGenomeFactory(size));
        BasicSpecies defaultSpecies = new BasicSpecies();
        defaultSpecies.setPopulation(result);
        result.getSpecies().add(defaultSpecies);

        // Create a new population of random networks.
        for (int i = 0; i < POPULATION_SIZE; i++) {
            final DoubleArrayGenome genome = new DoubleArrayGenome(size);
            network.reset(rnd);
            System.arraycopy(network.getLongTermMemory(), 0, genome.getData(), 0, size);
            defaultSpecies.add(genome);
        }

        // Set the genome factory to use the double array genome.
        result.setGenomeFactory(new DoubleArrayGenomeFactory(size));

        return result;

    }

    public static void main(final String[] args) {
        final ModelIris prg = new ModelIris();
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

            final RBFNetworkGenomeCODEC codec = new RBFNetworkGenomeCODEC(4, 4, 3);

            final List<BasicData> trainingData = ds.extractSupervised(0,
                    codec.getInputCount(), codec.getRbfCount(), codec.getOutputCount());

            Population pop = initPopulation(rnd, codec);

            ScoreFunction score = new ScoreRegressionData(trainingData);

            BasicEA genetic = new BasicEA(pop, score);
            genetic.setCODEC(codec);
            genetic.addOperation(0.7, new Splice(codec.size() / 5));
            genetic.addOperation(0.3, new MutatePerturb(0.1));


            performIterations(genetic, 100000, 0.05, true);

            RBFNetwork winner = (RBFNetwork) codec.decode(genetic.getBestGenome());

            queryOneOfN(winner, trainingData, species);


        } catch (Throwable t) {
            t.printStackTrace();
        }


    }
}
