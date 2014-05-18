package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig;
import com.heatonresearch.aifh.examples.capstone.model.milestone1.NormalizeTitanic;
import com.heatonresearch.aifh.examples.capstone.model.milestone1.TitanicStats;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;
import com.heatonresearch.aifh.learning.TrainPSO;
import com.heatonresearch.aifh.learning.score.ScoreFunction;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/17/14
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class FitTitanic {

    private RBFNetwork bestNetwork;
    private double bestScore;
    private CrossValidate cross;

    public void trainFold(int k, CrossValidateFold fold) {
        int noImprove = 0;
        double localBest = 0;
        List<BasicData> training = fold.getTrainingSet();
        List<BasicData> validation = fold.getValidationSet();

        GenerateRandom rnd = new MersenneTwisterGenerateRandom();
        RBFNetwork[] particles = new RBFNetwork[TitanicConfig.ParticleCount];
        for(int i=0;i<particles.length;i++) {
            particles[i] = new RBFNetwork(TitanicConfig.InputFeatureCount,TitanicConfig.RBF_COUNT,1);
            particles[i].reset(rnd);
        }

        if( bestNetwork==null ) {
        bestNetwork = new RBFNetwork(TitanicConfig.InputFeatureCount,TitanicConfig.RBF_COUNT,1);
        }

        ScoreFunction score = new ScoreTitanic(training);
        ScoreFunction scoreValidate = new ScoreTitanic(validation);
        boolean done = false;

        TrainPSO train = new TrainPSO(particles,score);
        int iterationNumber = 0;
        StringBuilder line = new StringBuilder();

        do {
            iterationNumber++;

            train.iteration();

            RBFNetwork best = (RBFNetwork)train.getBestParticle();
            best.getLongTermMemory().clone();

            double trainingScore = train.getLastError();
            double validationScore = scoreValidate.calculateScore(best);

            if( validationScore>bestScore ) {
                System.arraycopy(best.getLongTermMemory(),0,this.bestNetwork.getLongTermMemory(),0,best.getLongTermMemory().length);
                this.bestScore = validationScore;
            }

            if( validationScore>localBest ) {
                noImprove = 0;
                localBest = validationScore;
            } else {
                noImprove++;
            }

            line.setLength(0);
            line.append("Fold #");
            line.append(k+1);
            line.append(", Iteration #");
            line.append(iterationNumber);
            line.append(": training correct: ");
            line.append(trainingScore);
            line.append(", validation correct: ");
            line.append(validationScore);
            line.append(", no improvement: ");
            line.append(noImprove);

            if( noImprove>TitanicConfig.AllowNoImprovement ) {
                done = true;
            }

            System.out.println(line.toString());
        } while (!done);

        fold.setScore(localBest);
    }



    public void process(File dataPath) {
        File trainingPath = new File(dataPath,"train.csv");
        File testPath = new File(dataPath,"test.csv");

        GenerateRandom rnd = new MersenneTwisterGenerateRandom();

        try {

            TitanicStats stats = new TitanicStats();
            NormalizeTitanic.analyze(stats, trainingPath);
            NormalizeTitanic.analyze(stats, testPath);

            List<BasicData> training = NormalizeTitanic.normalize(stats, trainingPath, null,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish);
            this.cross = new CrossValidate(TitanicConfig.FoldCount,training,rnd);


            for(int k=0;k<cross.size();k++) {
                System.out.println("Cross validation fold #" + (k+1) + "/" + cross.size());
                trainFold(k,cross.getFolds().get(k));
            }

            System.out.println("Crossvalidation summary:");
            int k = 1;
            for(CrossValidateFold fold: cross.getFolds()) {
                System.out.println("Fold #" + k + ": " + fold.getScore());
                k++;
            }

            System.out.print("Final, crossvalidated score:" +  cross.getScore());

        } catch(IOException ex) {
            ex.printStackTrace();
        }
    }

    public RBFNetwork getBestNetwork() {
        return bestNetwork;
    }

    public double getBestScore() {
        return bestScore;
    }

    public CrossValidate getCrossvalidation() {
        return this.cross;
    }

    public static void main(String[] args) {
        if( args.length!=1 ) {
            System.out.println("Please call this program with a single parameter that specifies your data directory.");
            System.exit(0);
        }

        File dataPath = new File(args[0]);

        FitTitanic fit = new FitTitanic();
        fit.process(dataPath);
    }
}
